"use strict";

require("./styles.scss");
const { Elm } = require("./Main");

WebFont.load({ google: { families: ["Grenze Gotisch", "VT323"] } });

var totalAssetsToLoad = 0;
var numAssetsLoaded = 0;
const loadedSounds = {};

function allAssetsLoaded() {
    // return to stay on loading screen
    return;
    // note, this replaces the full <body>
    let app = Elm.Main.init({ flags: 6 });
    app.ports.playSound.subscribe((key) => {
        loadedSounds[key].play();
    });
    loadedSounds["music/soundtrack"].play();
}

function assetLoaded(e) {
    // console.log("loaded", e.target.src);
    numAssetsLoaded++;
    if (numAssetsLoaded === totalAssetsToLoad) {
        // console.log("all assets loaded");
        allAssetsLoaded();
    } else {
        // console.log(`loaded ${numAssetsLoaded} of ${totalAssetsToLoad} assets`);
    }
}

const sounds = {
    "music/soundtrack": {
        exts: ["wav"],
        waitForLoad: true,
        loop: true,
        volume: 0.3
    },
    "sfx/fight": { exts: ["wav"], volume: 0.3 },
    "sfx/select": { exts: ["wav"], volume: 0.2 },
    "sfx/draw": { exts: ["wav"], volume: 0.3 },
    "sfx/win": { exts: ["wav"], volume: 0.4 }
};

Object.entries(sounds).forEach(loadSound);

function loadSound([key, { waitForLoad, exts, loop, volume }]) {
    if (waitForLoad) totalAssetsToLoad++;
    let sound = new Howl({
        src: exts.map((ext) => key + "." + ext),
        loop: loop || false,
        volume: volume || 1
    });
    if (waitForLoad)
        sound.once("load", assetLoaded.bind(null, { target: { src: key } }));
    // console.log("loading sound", key);
    loadedSounds[key] = sound;
}
