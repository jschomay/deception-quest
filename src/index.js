"use strict";

require("./styles.scss");

WebFont.load({
    google: {
        families: ["Grenze Gotisch", "VT323"]
    }
});

// start up elm app
const { Elm } = require("./Main");
var app = Elm.Main.init({ flags: 6 });
