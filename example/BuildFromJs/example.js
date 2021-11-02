//import { Nest, Tron, Ports, run } from "./builder.js";

//export function buildExample(ports : Ports) : void {
function buildExample(ports) {

    const companion =
        { value : 42
        , live : 14
        , test : () => { console.log('test'); }
        , toggle : false
        , text : 'aaa'
        , xy : { x: 20, y : 13 }
        , colorA : { red: 0.8, green : 0, blue : 0.5, alpha : 0.5 } // '#ff6633'
        , colorB : { red: 0.2, green : 0.3, blue : 0.4 }
        , product : 'idea'
        , buttonA : () => { console.log('A'); }
        , buttonB : () => { console.log('B'); }
        , buttonC : () => { console.log('C'); }
        , buttonD : () => { console.log('D'); }
        , innerKnobA : 200
        , innerKnobB : 30
        };

    const tron = new Tron(ports, companion);

    tron.num('value', 0, 42).onChange((val) => { console.log(companion.value, val); });
    tron.num('live', 0, 42).live().onChange((val) => { console.log(companion.live, val); });
    tron.button('test');
    tron.toggle('toggle').onChange((val) => { console.log(companion.toggle, val); });
    tron.text('text').onChange((val) => { console.log(companion.text, val); });
    tron.color('colorA').onChange((val) => { console.log(companion.colorA, val); });
    tron.color('colorB').live().onChange((val) => { console.log(companion.colorB, val); });
    tron.xy('xy', { x : 0, y : 0 }, { x : 42, y : 42 }).onChange((val) => { console.log(companion.xy, val); });
    tron.xy('xy', { x : 0, y : 0 }, { x : 42, y : 42 }).live().onChange((val) => { console.log(companion.xy, val); });
    tron.choice('product', [ 'pycharm', 'idea', 'webstorm', 'rubymine' ]).onChange((val) => { console.log(companion.product, val); });
    tron.buttons('buttons', [ 'buttonA', 'buttonB', 'buttonC', 'buttonD' ]);

    const nest = tron.nest('knobs');
    nest.num('innerKnobA', 0, 1000).onChange((val) => { console.log(companion.innerKnobA, val); });
    nest.num('innerKnobB', 0, 1000).onChange((val) => { console.log(companion.innerKnobB, val); });

    tron.run();
}