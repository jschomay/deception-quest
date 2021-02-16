"use strict";

require("./styles.scss");

const projectId = "XwKSVJe3dg3ZXdm6Rf9T";
const projectRef = firebase.firestore().collection("projects").doc(projectId);

const manifestRef = projectRef.collection("manifest");
const rulesRef = projectRef.collection("rules");

// start up elm app
const { Elm } = require("./Main");
var app = Elm.Main.init({ flags: 6 });

// fetch and import manifest data
manifestRef.onSnapshot(
    (snapshot) => {
        // TODO check if change type is "remove" and respond accordingly
        // for now this just sends any new/changed docs
        app.ports.addEntities.send(
            snapshot
                .docChanges()
                .map((c) => c.doc.data())
                .filter(({ entity }) => !!entity)
        );
    },
    (e) => console.error(e)
);

// fetch and import rules data
rulesRef.onSnapshot(
    (snapshot) => {
        // TODO check if change type is "remove" and respond accordingly
        // for now this just sends any new/changed docs
        app.ports.addRules.send(
            snapshot
                .docChanges()
                .map((c) => c.doc.data())
                .filter(({ rule_id }) => !!rule_id)
        );
    },
    (e) => console.error(e)
);
