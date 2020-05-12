
### rot.js in ClojureScript

This is the tutorial from rot.js https://ondras.github.io/rot.js/hp/ implemented in ClojureScript

Shows off some features of Clojure, including
* host-language interop
* async processes with channels
* multi-method dispatch
* concurrency-safe mutable data structures

Built using shadow-cljs http://doc.shadow-cljs.org/ from the template at https://github.com/minimal-xyz/minimal-shadow-cljs-browser


----

### Develop

Install shadow-cljs: https://shadow-cljs.github.io/docs/UsersGuide.html#_installation

Shadow-cljs allows javascript interop with npm packages. Install rot-js in the project folder with

```bash
npm install rot-js
```

`:dev-http` specifies that `target/` will be served at http://localhost:8080 .

Copy index.html and style.css from assets/ to target/

### REPL

After page is loaded, you may also start a REPL connected to browser. 

See: https://shadow-cljs.github.io/docs/UsersGuide.html#_repl_2

### VS Code

Visual Studio Code with the Calva plugin https://calva.io/ can start shadow-cljs and automatically connects a REPL.

In VS Code, ctrl+alt+c ctrl+alt+j to jack in, and choose :app build

For details see https://shadow-cljs.github.io/docs/UsersGuide.html#_calva_vs_code

### License

MIT
