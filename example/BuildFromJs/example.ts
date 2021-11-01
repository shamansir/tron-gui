import { Nest, Tron, Ports, run } from "./builder.js";

export function buildExample(ports : Ports) : void {

    const companion =
        { value : 42
        , test : () => { console.log('test'); }
        };

    const tron : Tron = new Tron();
    tron.num(companion, 'value', 0, 42).onChange((val) => { console.log(companion.value, val); });
    tron.button(companion, 'test');

    run(ports, tron);
}