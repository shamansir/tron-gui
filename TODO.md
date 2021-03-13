* New design;
* Helpers, optional to use, based on the examples, like `detachable`, `communicateJS`...
* Rename `Gui` module to `Tron`;
* Pagination in panels which don't fit the inner buttons;
* Fix URLs for icons not to require `assets/{icon}_{theme}.svg` address;
* Multiple-choice control;
* Include links to the examples in the docs;
* Include minimal "OneControl" example into the docs;
* Get rid of functions in the `Model`:
    * do not store tree in the `Gui msg`, build it every time;
    * store the actual messages for the current value in the controls, not the handlers;