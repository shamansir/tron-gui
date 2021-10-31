type Color = { hex : string } | { r : number, g : number, b : number, a : number } ;


type ControlType = "none" | "num" | "xy" | "text" | "color" | "nest" | "choice" | "toggle" | "button";


type Value = string | number | [number, number] | Color;


interface ChangeHandler {
    (value : Value): void
}


class Control {
  readonly companionProperty? : string = undefined;
  readonly id? : string = undefined;
  readonly type : ControlType = "none";
  readonly changeHandlers : ChangeHandler[] = [];
  readonly finishChangeHandlers : ChangeHandler[] = [];
  onChange(handler : ChangeHandler) : Control { return this; };
  onFinishChange(handler : ChangeHandler) : Control { return this; };
  sendChange(value: Value) : Control { return this; }
}

class Nest extends Control {
  controls : Control[] = [];
  addOne(control : Control) : Nest { return this; }
  add(controls : Control[]) : Nest { return this; }
}


type Tron = Nest;


function build(tron : Tron): void {

}