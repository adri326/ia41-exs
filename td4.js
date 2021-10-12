#! /bin/env node

const assert = require("assert");

let file_tree = {
    rep1: {
        rep3: {auto: 0, moto: 0, velo: 0},
        lolo: 0,
        lulu: 0,
    },
    rep2: {
        rep4: {fado: 0, java: 0, torot: 0},
        chato: 0,
        gato: 0,
        rado: 0,
        rato: 0,
    },
    tata: 0,
    toto: 0,
    tutu: 0,
};

const file_dfs = module.exports.file_dfs = function file_dfs(tree) {
    let res = [];
    for (let node in tree) {
        if (tree[node] === 0) { // leaf
            if (node.includes("to")) res.push(node);
        } else {
            res = res.concat(file_dfs(tree[node]));
        }
    }
    return res;
}

assert.deepEqual(file_dfs(file_tree).sort(), ["auto", "chato", "gato", "moto", "rato", "torot", "toto"]);
assert.deepEqual(file_dfs(file_tree), ["auto", "moto", "torot", "chato", "gato", "rato", "toto"]);

const file_bfs = module.exports.file_bfs = function file_bfs(tree) {
    let open = [tree];
    let res = [];
    while (open.length) {
        let current = open.shift();
        if (typeof current === "string") { // leaf
            if (current.includes("to")) res.push(current);
        } else {
            for (let node in current) {
                if (current[node] === 0) open.push(node);
                else open.push(current[node]);
            }
        }
    }
    return res;
}

assert.deepEqual(file_bfs(file_tree).sort(), ["auto", "chato", "gato", "moto", "rato", "torot", "toto"]);
assert.deepEqual(file_bfs(file_tree), ["toto", "chato", "gato", "rato", "auto", "moto", "torot"]);
