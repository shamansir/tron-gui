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

    toJson(): any {
        switch (this.type) {
            case "num" :
                return {
                    type : "slider",
                    min : this.props.min,
                    max : this.props.max,
                    step : this.props.step,
                    current : this.companion && this.companionProperty ? this.companion[this.companionProperty] : null
                };
            case "button" :
                return {
                    type : "button"
                };
            default :
                return {}
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

    num(companion : any, companionPropety : string, min : number = 0, max : number = 100, step : number = 1) : Control
        { return this.add(new Control("num", { min, max, step }, companion, companionPropety)); };
    button(companion : any, companionPropety : string) : Control
        { return this.add(new Control("button", { }, companion, companionPropety)); };

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