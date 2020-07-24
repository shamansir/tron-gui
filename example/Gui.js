


const propNameFromPath = (path) => {
  return path
    .concat([])
    .reverse()
    .join('_');
}


const lastPropName = (path) => {
  const lastChunk = path[path.length - 1];
  return lastChunk ? lastChunk.item : '';
}

const sendProxy = (origSend) => {
  return (property) => {
    return (value) => {
        origSend({ path : property.path, type_ : property.type, value });
    };
  }
};


const applyDefinition = (config, definition, send) => {

  function setProperty(label, property) {
    const propName = propNameFromPath(property.path);

    if (property.type == 'gui') {
      property.nest.gui.forEach((component) => {
        setProperty(component.label, component.property);
      });
    } else if (property.type == 'button') {
      const sender = send(property);
      config[propName] = () => { sender('button'); }
    } else if (property.type == 'toggle') {
      config[propName] = (property.current == 'on');
    } else if (property.type != 'ghost') {
      config[propName] = property.current;
    }
  }

  definition.gui.forEach((component) => {
    setProperty(component.label, component.property);
  });

}

const Config = function(definition, send) {
  applyDefinition(this, definition, send);
};

function start(document, definition, origSend) {

  const send = sendProxy(origSend);

  const gui = new dat.GUI(/*{ load: JSON }*/);
  const config = new Config(definition, send);

  function addControl(root, label, property) {

    const propName = propNameFromPath(property.path);

    if (property.type == 'gui') {
      const nestedGui = root.addFolder(label);

      property.nest.gui.forEach((component) => {
        addControl(nestedGui, component.label, component.property);
      });

      if (property.expanded) {
        nestedGui.open();
      }

    } else if (property.type == 'choice') {

      let optionsObj = {};
      property.options.forEach(({ label, index }) => {
        optionsObj[label] = index;
      });

      const choice = root.add(config, propName, optionsObj).name(label);
      const sender = send(property);
      choice.onFinishChange((value) => {
        sender(parseInt(value));
      });

    } else if (property.type == 'button') {

      root.add(config, propName).name(label);

    } else if (property.type == 'toggle') {

      const toggle = root.add(config, propName).name(label);
      const sender = send(property);
      toggle.onFinishChange((value) => {
        sender(value);
      });

    } else if (property.type == 'slider') {

      const slider = root.add(config, propName)
        .min(property.min)
        .max(property.max)
        .step(property.step)
        .name(label);
      const sender = send(property);
      slider.onFinishChange((value) => {
        sender(value);
      });

    } else if (property.type == 'text') {

      const text = root.add(config, propName).name(label);
      const sender = send(property);
      text.onFinishChange((value) => {
        sender(value);
      });

    } else if (property.type == 'color') {

      const color = root.addColor(config, propName).name(label);
      const sender = send(property);
      color.onFinishChange((value) => {
        sender(value);
      });

    }

  }

  definition.gui.forEach((component) => {
    addControl(gui, component.label, component.property);
  });

  document.querySelector('.dg.ac').classList.add('hide-on-space');

  return {
    gui,
    config,
    update : (nextDefinition) => applyDefinition(config, nextDefinition, send)
  };
}

module.exports = start;

window.startGui = start;
