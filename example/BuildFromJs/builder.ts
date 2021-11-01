type Color = { hex : string } | { r : number, g : number, b : number, a : number } ;


type ControlType = "none" | "num" | "xy" | "text" | "color" | "nest" | "choice" | "toggle" | "button";


type Value = string | number | [number, number] | Color;


interface ChangeHandler {
    (value : Value): void
}


interface SendTreePort {
    (tree : Tron): void;
}


interface ReceiveValuePort {
    (path : string[], value : Value, type : ControlType): void;
}


interface SendValuePort {
    (control : Control): void;
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

    toJson(): any { return {}; }
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
}


export class Ports {
    sendTree : SendTreePort;
    sendValue : SendValuePort;
    receiveValue : ReceiveValuePort;
}


export class Tron extends Nest {
    constructor() { super('root'); }
}


export function build(ports : Ports, root : Tron): Tron {
    return root;
}