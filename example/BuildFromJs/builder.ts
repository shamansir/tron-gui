type Color = { hex : string } | { r : number, g : number, b : number, a : number } ;


type ControlType = "none" | "num" | "xy" | "text" | "color" | "nest" | "choice" | "toggle" | "button";


type Value = string | number | [number, number] | Color;


interface ChangeHandler {
    (value : Value): void
}


type TreeJson = any;


type ValueJson = any;


type PortValueIn =
    { path : [ number, string ][]
    , value : ValueJson
    , stringValue : string
    , type_ : string
    }


type PortValueOut =
    { path : [ number, string ][]
    , value : ValueJson
    }


interface SendTreePort {
    send(tree : TreeJson): void;
}


interface ReceiveValuePort {
    subscribe (callback: ({ clientId : string, value : PortValueIn }) => void): void;
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
            return this;
        };
    //onFinishChange(handler : ChangeHandler) : Control { return this; };
    sendChange(value: Value) : Control {
            for (const changeHandler in this.changeHandlers) {
                this.changeHandlers[changeHandler](value);
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
            return this;
        };
    addMany(controls : Control[]) : Control
        {
            this.controls.concat(controls);
            return this;
        };

    num(companion : any, companionPropety : string, min : number = 0, max : number = 100, step : number = 1) : Control
        { return this.add(new Control("num", { min, max, step }, companion, companionPropety)); };
    button(companion : any, companionPropety : string) : Control
        { return this.add(new Control("button", { }, companion, companionPropety)); };

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
}


export function run(ports : Ports, root : Tron): Tron {
    // TODO: subscribe to changes before
    ports.build.send(root.toJson());
    return root;
}