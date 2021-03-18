* New design;
* Pagination in panels which don't fit the inner buttons;
* Re-check the examples to work;
* Fix URLs for icons not to require `assets/{icon}_{theme}.svg` address;
* Multiple-choice control;
* Include links to the examples in the docs;
* Fix `cols` vs `rows` to be bound to docking orientation;
* Include minimal `OneControl` example into the docs, as well as `Basic` one and `ReportToJs*` ones;
* Consider having `Nil (Property msg)` instead of just `Nil`, so that any property could be hidden, but not absent in the tree;
* Get rid of functions in the `Model`:
    * do not store tree in the `Gui msg`, build it every time;
    * store the actual messages for the current value in the controls, not the handlers;
    * for `.over`, traverse two trees with the same structure (don't forget about ghosts) and move transient states between them;
