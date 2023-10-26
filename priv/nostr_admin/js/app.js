import * as mithril from "./mithril.js";
import * as header from "./views/Header.js";
import * as footer from "./views/Footer.js";
import * as dashboard from "./views/Dashboard.js";

var app_main = document.getElementById("application");

var Clients = function(initialVnode) {
    var submenu = function() {
        return document.getElementById("submenu");
    };
    
    var menu = function() {
        m.mount(submenu(), () => {
            return m("", {}, [
                m("a", {class: "link dim white dib mr3", href: "#!/client/new"}, "create")
            ])
        })
    };
    
    var table_header = function() {
        return m("div", {class: "center dt dt--fixed w-50 pa1"},  [
            m("b", {class: "dtc"}, ["id"]),
            m("b", {class: "dtc"}, ["connections"]),
            m("b", {class: "dtc"}, ["users"]),
        ])
    };
    var table_row = function(identifier, connections, users) {
        return m("div", {class: "center dt dt--fixed w-50 pa1"},  [
            m("", {class: "dtc"}, [identifier]),
            m("", {class: "dtc"}, [connections]),
            m("", {class: "dtc"}, [users]),
        ])        
    }
    return {
        view: function(vnode) {
            return [table_header(), table_row("test", "1024", "12")];
        }
    }
}

var Relays = {
    view: function() {
        return m("", [
            "relays"
        ])
    }
}

var Stats = {
    view: function() {
        return m("", [
            "stats"
        ])
    }
}

var Configuration = {
    view: function() {
        return m("", [
            "configuration"
        ])
    }
}

m.route(app_main, "/", {
    "/": dashboard.Dashboard,
    "/clients": Clients,
    "/relays": Relays,
    "/stats": Stats,
    "/configuration": Configuration,
})
