import QtQuick 2.0
import QtGraphicalEffects 1.0

Item {
    id: c;
    width: tileSize;
    height: tileSize;
    clip: true;

    property double tileSize : 100;
    property double internalTileSize : 100;

    property var entryA : null;
    property var exitA : null;
    property double volume : 0;
    property color colour : 'green';

    property string _maskSource : '';
    property int _maskRotation : 0;
    property string _maskLetter : '';

    function setup() {
        var source = '', rotation = 0, letter = '';
        if (rotate(c.entryA, 180) == c.exitA) {
            source = 'straight';
            rotation = c.entryA;
        }
        else if (rotate(c.entryA, 90) == c.exitA) {
            source = 'corner';
            rotation = c.entryA;
        }
        else if (rotate(c.exitA, 90) == c.entryA) {
            source = 'corner';
            rotation = c.exitA;
        }
        else if (isOrtho(c.entryA) && !isOrtho(c.exitA)) {
            source = 'end';
            rotation = c.entryA;
            letter = 'E';
        }
        else if (isOrtho(c.exitA) && !isOrtho(c.entryA)) {
            source = 'end';
            rotation = c.exitA;
            letter = 'S';
        }
        else {
            source = 'full';
            letter = 'X';
        }
        c._maskSource = source;
        c._maskRotation = rotation;
        c._maskLetter = letter;
    }
    Component.onCompleted: setup();
    onEntryAChanged: setup();
    onExitAChanged: setup();

    function rotate(a, d) {
        return isOrtho(a) ? (a+d)%360 : -1;
    }

    function isOrtho(a) {
        return a == 0 || a == 90 || a == 180 || a == 270;
    }

    DropShadow {
        anchors.fill: c;
        radius: 0.025*c.tileSize;
        spread: 0.8;
        fast: true;
        color: 'black';
        opacity: 0.2;
        source: mask;
    }
    LinearGradient {
        function steer(n, dir) {
            switch (dir) {
            case 0: return {x: 0.5, y: n};
            case 90: return {x: 1-n, y: 0.5};
            case 180: return {x: 0.5, y: 1-n};
            case 270: return {x: n, y: 0.5};
            }
            return {x: 0.5, y: 0.5};
        }

        function point(p) {
            return Qt.point(p.x*c.width, p.y*c.height);
        }

        function calc(vol, t, ena, exa) {
            var band = 0.3;
            var t2 = t + band;
            if (!isOrtho(ena)) {
                vol = 0.4+vol*0.6;
                ena = rotate(exa, 180);
            }
            else if (!isOrtho(exa)) {
                vol = vol*0.6;
                exa = rotate(ena, 180);
            }
            exa = rotate(exa, 180);
            if (vol < t) {
                return point(steer(vol, ena));
            }
            else if (vol < t2) {
                var v = (vol-t)/band;
                var a = steer(t, ena);
                var b = steer(t2, exa);
                return point({x: a.x*(1-v)+b.x*v, y: a.y*(1-v)+b.y*v});
            }
            else {
                return point(steer(vol, exa));
            }
        }

        id: grad;
        anchors.fill: parent;
        start: calc(c.volume-0.1, 0.3, c.entryA, c.exitA);
        end: calc(c.volume, 0.4, c.entryA, c.exitA);
        gradient: Gradient {
            GradientStop { position: 0.0; color: colour; }
            GradientStop { position: 1.0; color: Qt.rgba(0,0,0,0.1); }
        }
        source: mask; 
    }
    Item {
        id: mask;
        anchors.fill: parent; visible: false;
        layer.enabled: true;
        Image {
            anchors.fill: parent;
            rotation: c._maskRotation;
            source: c._maskSource + '_mask.svg';
            sourceSize.width: c.internalTileSize;
            sourceSize.height: c.internalTileSize;
        }
    }
    Item {
        anchors.fill: parent;
        visible: c._maskLetter != '';
        Text {
            anchors.centerIn: parent;
            font.pixelSize: 0.2*c.tileSize;
            text: c._maskLetter;
            color: c.colour.r + c.colour.g + c.colour.b > 1.5 ?
                Qt.darker(c.colour, 2) : Qt.lighter(c.colour, 2);
        }
    }
}
