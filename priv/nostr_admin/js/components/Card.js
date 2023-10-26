import * as mithril from "../mithril.js";

// export var Card = {
//     view: function(title, target) {
//         return m("div", {class: "mw5 bg-white br3 pa3 pa4-ns mv3 ba b--black-10 outline w-25 pa3 mr2 fl w-25 pa2"}, [
//             m("div", {class: "tc"}, [
//                 m("h1", {class: "f4"}, [
//                     m("a", {href: target}, title)
//                 ]),
//                 m("hr", {class: "mw3 bb bw1 b--black-10"}, "")
//             ]),
//             m("p", {class: "lh-copy measure center f6 black-70"}, [
//                 "..."
//             ])
//         ])
//     }
// }

export var Card = {
    view: function(title, target) {
        return m("div", {class:"mw5 bg-white br3 pa3 pa4-ns mv3 ba b--black-10 mr2"}, [
            m("div", {class: "tc"}, [
                m("img", {src: "http://tachyons.io/img/avatar_1.jpg", class:"br-100 h4 w4 dib ba b--black-05 pa2", title:"Photo of a kitty staring at you"}),
                m("h1", {class: "f3 mb2"}, [title]),
                m("h2", {class: "f5 fw4 gray mt0"}, ["test"])
            ])
        ])
    }
}
