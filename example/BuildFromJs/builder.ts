type Color = { hex : string } | { r : number, g : number, b : number, a : number } ;


type ControlType = "none" | "num" | "xy" | "text" | "color" | "nest" | "choice" | "toggle" | "button";


type Value = string | number | [number, number] | Color;


interface ChangeHandler {
    (value : Value): void
}


type Path = [ number, string ][]


type LabelPath =  string[]


type TreeJson = any;


type ValueJson = any;


type PortValueIn =
    { path : Path
    , value : ValueJson
    , stringValue : string
    , type_ : string
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
    readonly companion? : any;
    readonly companionProperty? : string;
    label? : string;
    readonly type : ControlType;
    readonly props : any;
    //readonly path : string[];

    constructor(type: ControlType, props : any, companion? : any, companionProperty?: string, label?: string) {
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
            console.log('add handler for ', this.label);
            return this;
        };
    //onFinishChange(handler : ChangeHandler) : Control { return this; };
    handle(value: Value) : Control {
            if (this.companion && this.companionProperty && this.companion[this.companionProperty]) {
                if (this.type == "button") {
                    this.companion[this.companionProperty]();
                } else {
                    this.companion[this.companionProperty] = value;
                }

            }
            for (const changeHandler in this.changeHandlers) {
                this.changeHandlers[changeHandler](value);
            }
            return this;
        }

    send(ports : Ports, value: Value) : Control
        {
            return this;
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
            case "num" :
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


export class Nest extends Control {
    controls : Control[] = [];

    constructor(label : string, companion? : any, companionProperty?: string) {
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

    num(companion : any, companionProperty : string, min : number = 0, max : number = 100, step : number = 1) : Control
        { return this.add(new Control("num", { min, max, step }, companion, companionProperty)); };
    button(companion : any, companionProperty : string) : Control
        { return this.add(new Control("button", { }, companion, companionProperty)); };
    toggle(companion : any, companionProperty : string) : Control
        { return this.add(new Control("toggle", { }, companion, companionProperty)); };
    text(companion : any, companionProperty : string) : Control
        { return this.add(new Control("text", { }, companion, companionProperty)); };
    xy(companion : any, companionProperty : string
      , min : { x : number, y : number } = { x : 0, y : 0 }
      , max : { x : number, y : number } = { x : 100, y : 100 }
      , step : { x : number, y : number } = { x : 1, y : 1 }) : Control
        { return this.add(new Control("xy", { min, max, step }, companion, companionProperty)); };
    color(companion : any, companionProperty : string) : Control
        { return this.add(new Control("color", { }, companion, companionProperty)); };
    choice(companion : any, companionProperty : string, items : string[]) : Control
        { return this.add(new Control("choice", { }, companion, companionProperty)); };
    buttons(companion : any, properties : string[]) : Control
        { return this; /* FIXME: implement */ };
    nest(label : string, companion? : any, companionProperty? : string) : Nest
        { return this.add(new Nest(label, companion, companionProperty)) as Nest; };

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


export class Tron extends Nest {
    constructor() { super('root'); }

    run(ports : Ports): Tron {
        console.log(this.toJson());
        ports.build.send(this.toJson());
        ports.transmit.subscribe(({ update }) => {
            let maybeControl = this.findExact(update.path);
            if (maybeControl != null) {
                maybeControl.handle(update.value);
            }
        });
        return this;
    }
}