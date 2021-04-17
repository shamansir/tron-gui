* New design;
* Fix pagination UX and distribution over pages;
* Multiple-choice control;
* Include links to the examples in the docs;
* Share examples somewhere;
* Include minimal `OneControl` example into the docs, as well as `Basic` one and `ReportToJs*` ones;
* Include separate `Random` example to only utilize random generator, and, may be, test the detachable functionality, if the server is started;
* Consider having `Nil (Property msg)` instead of just `Nil`, so that any property could be hidden, but not absent in the tree;
* Get rid of functions in the `Model`:
    * do not store tree in the `Gui msg`, build it every time;
    * store the actual messages for the current value in the controls, not the handlers (i.e. just `msg` instead of `v -> msg`);
    * for `.over`, traverse two trees with the same structure (don't forget about ghosts) and move transient states between them;
* Check `indexedMap` usages, so that usage of the index is kept to minimum for nested items;
* Fix knobs to use current value when user starts dragging;
* Do not store cell size in the `Gui msg`, it should be recalculated every time;
* Do not store dock in the `Gui msg`, it should be recalculated every time;
* _Active_ condition for a button, some effect for when it is pressed;
* Detachable: Add clientId to the URL when it was generated (so that detaching won't lose changes);
* Add some indication of the WS server status to the examples;
