import * as mithril from "../mithril.js";
import * as card from "../components/Card.js";

export var Dashboard = {
    view: function() {
        return m("div", {id: "dashboard", class: "flex"}, [
            card.Card.view("clients", "#!/clients"),
            card.Card.view("relays", "#!/relays"),
            card.Card.view("stats", "#!/stats"),
            card.Card.view("configuration", "#!/configuration"),
        ])
    }
}
