import { Elm } from './Main.elm'
import pink from './images/pink.png'
import red from './images/red.png'

Promise.all([
    fetch("http://localhost:8080/maps/map1.tmx").then((res) => res.text())
])
    .then(([
        map1Tmx
    ]) => {
        Elm.Main.init({
            node: document.getElementById('app'),
            flags: {
                pink,
                red,
                map1Png: "http://localhost:8080/maps/map1.png",
                map1Tmx
            }
        })
    })
    .catch((err) => {
        console.error(err)
    })


