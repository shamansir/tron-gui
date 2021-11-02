type Color = { hex : string } | { r : number, g : number, b : number, a : number } ;


type ControlType = "none" | "slider" | "xy" | "text" | "color" | "nest" | "choice" | "toggle" | "button" | "attachment";


type ChoiceSelection = { selection : string, index : number };


type Value = string | number | [number, number] | Color | ChoiceSelection;


interface ChangeHandler {
    (value : Value): void
}


type Path = [ number, string ][]


type LabelPath =  string[]


type TreeJson = any;


type ValueJson = any;


type Companion = any;


type PortValueIn =
    { path : Path
    , value : ValueJson
    , stringValue : string
    , type_ : ControlType
    }


type PortValueOut =
    { path : Path
    , value : ValueJson
    }


interface SendTreePort {
    send(tree : TreeJson): void;
}


interface ReceiveValuePort {
    subscribe (callback: ({ client : string, update : PortValueIn }) => void): void;
}


interface SendValuesPort {
    send(values : PortValueOut[]): void;
}


export class Control {
    readonly companion? : Companion;
    readonly companionProperty? : string;
    label? : string;
    readonly type : ControlType;
    readonly props : any;
    //readonly path : string[];

    constructor(type: ControlType, props : any, companion? : Companion, companionProperty?: string, label?: string) {
        this.type = type;
        this.props = props;
        this.companion = companion;
        this.companionProperty = companionProperty;
        this.label = label || companionProperty;
    }

    name(name: string): Control { this.label = name; return this; }

    changeHandlers : ChangeHandler[] = [];
    //finishChangeHandlers : ChangeHandler[] = [];

    onChange(handler : ChangeHandler) : Control {
            this.changeHandlers.push(handler);
            return this;
        };
    //onFinishChange(handler : ChangeHandler) : Control { return this; };
    handle(value: Value) : Control {
        return this.handleAs(this.type, value);
    }
    handleAs(type : ControlType, value: Value) : Control {
        if (this.companion && this.companionProperty && (typeof this.companion[this.companionProperty] != 'undefined')) {
            if (type == "button") {
                this.handleAction();
            } else if (type == "choice") {
                this.assignValue((value as ChoiceSelection).selection);
            } else {
                this.assignValue(value);
            }

        }
        for (const changeHandler in this.changeHandlers) {
            this.changeHandlers[changeHandler](value);
        }
        return this;
    }
    handleAction() : void {
        this.companion[this.companionProperty]();
    }
    assignValue(value : Value) : void {
        this.companion[this.companionProperty] = value;
    }

    send(ports : Ports, value: Value) : Control {
        return this; // TODO
    }

    get() : Value | null {
            return this.companion && this.companionProperty ? this.companion[this.companionProperty] : null
        }

    live() : Control {
            if (this.props) {
                this.props.live = true;
            }
            return this;
        }

    toJson(): any {
        switch (this.type) {
            case "slider" :
                return {
                    type : "slider",
                    min : this.props.min,
                    max : this.props.max,
                    step : this.props.step,
                    live : this.props.live,
                    current : this.get()
                };
            case "toggle" :
                return {
                    type : "toggle",
                    current : this.get() ? "on" : "off"
                };
            case "xy" :
                return {
                    type : "xy",
                    minX : this.props.min.x,
                    minY : this.props.min.y,
                    maxX : this.props.max.x,
                    maxY : this.props.max.y,
                    stepX : this.props.step.x,
                    stepY : this.props.step.y,
                    live : this.props.live,
                    current : this.get()
                };
            case "text" :
                return {
                    type : "text",
                    current : this.get()
                };
            case "color" :
                return {
                    type : "color",
                    live : this.props.live,
                    currentRgba : this.get()
                };
            case "button" :
                return {
                    type : "button"
                };
            default :
                return { type : "none" }
        }
    }
}

export class Choice extends Control {
    options : Control[] = [];

    constructor(label : string, companion? : Companion, companionProperty?: string) {
        super("choice", {}, companion, companionProperty, label);
    }

    addOption(companionProperty? : string, label?: string) : Control
        {
            let option = new Control("button", { }, this.companion, companionProperty, label);
            this.options.push(option);
            return option;
        };

    toJson() {
        return {
            type : "choice",
            options : this.options.map(
                        (control, index) =>
                            ({ property : control.toJson(), index: index, label : control.label })
                    )
        };
    }
}


export class Nest extends Control {
    controls : Control[] = [];

    constructor(label : string, companion? : Companion, companionProperty?: string) {
            super("nest", {}, companion, companionProperty, label);
        }

    add(control : Control) : Control
        {
            this.controls.push(control);
            return control;
        };
    addMany(controls : Control[]) : Nest
        {
            this.controls.concat(controls);
            return this;
        };

    num(companionProperty : string, min : number = 0, max : number = 100, step : number = 1) : Control
        { return this.add(new Control("slider", { min, max, step }, this.companion, companionProperty)); };
    button(companionProperty : string) : Control
        { return this.add(new Control("button", { }, this.companion, companionProperty)); };
    toggle(companionProperty : string) : Control
        { return this.add(new Control("toggle", { }, this.companion, companionProperty)); };
    text(companionProperty : string) : Control
        { return this.add(new Control("text", { }, this.companion, companionProperty)); };
    xy(companionProperty : string
      , min : { x : number, y : number } = { x : 0, y : 0 }
      , max : { x : number, y : number } = { x : 100, y : 100 }
      , step : { x : number, y : number } = { x : 1, y : 1 }) : Control
        { return this.add(new Control("xy", { min, max, step }, this.companion, companionProperty)); };
    color(companionProperty : string) : Control
        { return this.add(new Control("color", { }, this.companion, companionProperty)); };
    choice(companionProperty : string, options : string[]) : Control
        {
            const choice = new Choice(companionProperty, this.companion, companionProperty);
            for (let optionIdx in options) {
                choice.addOption(null, options[optionIdx]);
            }
            return this.add(choice);
        };
    buttons(label : string, properties : string[]) : Nest
        {
            const nest = this.nest(label);
            for (let propertyIdx in properties) {
                nest.button(properties[propertyIdx])
            }
            return nest;
        };
    nest(label : string, companionProperty? : string) : Nest
        { return this.add(new Nest(label, this.companion, companionProperty)) as Nest; };

    find(path : LabelPath) : Control | null {
        if (path.length > 0) {
            let firstLabel = path[0];

            for (let controlIdx in this.controls) {
                if (this.controls[controlIdx].label == firstLabel) {
                    if (path.length == 1) {
                        return this.controls[controlIdx];
                    } else {
                        if (this.controls[controlIdx] instanceof Nest) {
                            return (this.controls[controlIdx] as Nest).find(path.slice(1));
                        } else {
                            return null;
                        }
                    }

                }
            }
            return null;
        } else return null;
    }

    findExact(path : Path) : Control | null {
        if (path.length > 0) {
            let [firstIdx, firstLabel] = path[0];

            for (let controlIdx in this.controls) {
                if ((parseInt(controlIdx) == firstIdx) && (this.controls[controlIdx].label == firstLabel)) {
                    if (path.length == 1) {
                        return this.controls[controlIdx];
                    } else {
                        if (this.controls[controlIdx] instanceof Nest) {
                            return (this.controls[controlIdx] as Nest).findExact(path.slice(1));
                        } else {
                            return null;
                        }
                    }

                }
            }
            return null;
        } else return null;
    }

    toJson() {
        return {
            type : "nest",
            nest : this.controls.map(
                        (control, index) =>
                            ({ property : control.toJson(), index: index, label : control.label })
                    )
        };
    }
}

export class Ports {
    build : SendTreePort;
    apply : SendValuesPort;
    transmit : ReceiveValuePort;
}


interface Attachments {
    [index : string]: Control
}


export class Tron extends Nest {
    readonly ports : Ports;
    attachments : Attachments = {};

    constructor(ports : Ports, companion : Companion)
        { super('root', companion);
          this.ports = ports;
        }

    // TODO: `Tron.attach`

    run(): Tron {
        console.log(this.toJson());
        this.ports.build.send(this.toJson());
        this.ports.transmit.subscribe(({ update }) => {
            const maybeControl = this.findExact(update.path);
            if (maybeControl != null) {
                maybeControl.handle(update.value);
            }
        });
        return this;
    }

    static pathAsString(path : Path) : string {
        return path.map((i) => i[1]).join('/');
    }

    static labelPathAsString(path : LabelPath) : string {
        return path.join('/');
    }

    attach(companionProperty: string, path : LabelPath, handler?: ChangeHandler) : Control | null {
        const pathAsString = Tron.labelPathAsString(path);

        if (!this.attachments[pathAsString]) {
            this.attachments[pathAsString] = new Control('attachment', {}, this.companion, companionProperty);
        };
        const attachment = this.attachments[pathAsString];
        if (handler) {
            attachment.onChange(handler);
        }
        return attachment;
    }

    listen() {
        this.ports.transmit.subscribe(({ update }) => {
            const pathAsString = Tron.pathAsString(update.path);
            console.log(pathAsString, update);
            const maybeControl = this.attachments[pathAsString];

            if (maybeControl != null) {
                maybeControl.handleAs(update.type_, update.value);
            }
        });
        return this;
    }
}