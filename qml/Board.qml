import QtQuick 2.0
import QtGraphicalEffects 1.0
import HsQML.Model 1.0

Item {
    id: c;
    property int tilesWide : 8;
    property int tilesHigh : 8;

    property double tileSize : Math.min(
        width / tilesWide,  height / tilesHigh);
    property var tileSource : newTileSource();
    property var gridModel : newGrid();
    property var plumbedGrid : gridModel.plumb();

    signal pressedComplete();

    property double countDown;
    NumberAnimation on countDown {
        from: 3; to: 0;
        duration: 9000;
        onStopped: volumeAnim.start(); 
    }

    property double n : 0;
    NumberAnimation on n {
        id: volumeAnim;
        from: 0; to: 100 + 0.1;
        duration: 5000 * (100 + 0.1);
        running: false;
    }

    Repeater {
        anchors.fill: parent;
        model: AutoListModel {
            mode: AutoListModel.ByKey;
            source: {
                var rs = plumbedGrid.leaks;
                rs.sort(function(a,b) {return a.idx-b.idx;});
                return rs;
            }
            equalityTest: function(a,b) {
                a.idx==b.idx && a.colour==b.colour && a.exitA==b.exitA;}
            keyFunction: function(a) {return a.x+'-'+a.y;}
        }

        Leak {
            tileSize: c.tileSize;
            tileX: modelData.x;
            tileY: modelData.y;
            colour: modelData.colour;
            exitBearing: modelData.exitA;
            trigger: n >= modelData.idx;
        }
    }

    Repeater {
        id: tiles;
        model: AutoListModel {
            source: plumbedGrid.plumbing;
            mode: AutoListModel.ByKey;
            equalityTest: function(a,b) {
                return a.idx==b.idx && a.entryA==b.entryA && a.exitA==b.exitA;}
            keyFunction: function(a) {return a.x+"-"+a.y;}
        }

        Plumb {
            tileSize: c.tileSize;
            x: modelData.x*c.tileSize;
            y: modelData.y*c.tileSize;
            entryA: modelData.entryA;
            exitA: modelData.exitA;
            colour: modelData.colour;
            volume: modelData.idx < 0 ?
                0 : Math.min(Math.max(c.n-modelData.idx,0),1.1);
        }
    }

    Text {
        visible: c.countDown > 0;
        anchors.centerIn: parent;
        text: Math.floor(c.countDown+1);
        font.pixelSize: c.tileSize*2;
        scale: 1+Math.ceil(c.countDown)-c.countDown;
        opacity: 0.5-(Math.ceil(c.countDown)-c.countDown)/2;
    }

    Rectangle {
        id: mouseView;
        property var mouse: realMouse;
        property int tileX: Math.floor(mouse.mouseX/c.tileSize);
        property int tileY: Math.floor(mouse.mouseY/c.tileSize);
        property bool tilePlacable:
            plumbedGrid.isPlacable(Math.floor(c.n), tileX, tileY);

        Connections {
            target: mouseView.mouse;
            onPressed: {
                if (mouseView.tilePlacable) {
                    c.gridModel = c.gridModel.place(
                        mouseView.tileX, mouseView.tileY, tileSource.top.tile);
                    c.tileSource = c.tileSource.next;
                }
                c.pressedComplete();
            } 
        }

        visible: mouseView.mouse.containsMouse;
        x: tileX*c.tileSize;
        y: tileY*c.tileSize;
        width: c.tileSize; height: c.tileSize;
        color: 'lightgray'; opacity: 0.5;
        Text {
            visible: !mouseView.tilePlacable;
            anchors.centerIn: parent;
            color: 'red';
            font.pixelSize: c.tileSize;
            text: 'X';
        }
    }

    Item {
        id: virtualMouse;
        property bool done : false;
        property bool active : true;
        property double oldTileX : 0;
        property double oldTileY : 0;
        property double tileX : 0;
        property double tileY : 0;
        property double dur : 0;
        Behavior on tileX {
            NumberAnimation {
                id: animX;
                duration: virtualMouse.dur;
            }
        }
        Behavior on tileY {
            NumberAnimation {
                id: animY;
                duration: virtualMouse.dur;
            }
        }
        NumberAnimation {
            id: xyAnim;
            duration: virtualMouse.dur;
            onStopped: virtualMouse.pressed();
        }
        function pick() {
            if (done) return;
            gridModel.pick(c.tileSource, tileX, tileY, Math.floor(c.n));
        }
        focus: true;
        Keys.enabled: true;
        Keys.onSpacePressed: done = true;
        function move(pos) {
            dur = 300*Math.max(1,Math.sqrt(
                (tileX-pos.x)*(tileX-pos.x)+(tileY-pos.y)*(tileY-pos.y)));
            tileX = pos.x;
            tileY = pos.y;
            xyAnim.start();
        }
        Connections {
            target: c;
            onPressedComplete: virtualMouse.pick();
        }
        Connections {
            target: c.gridModel;
            onPicked: virtualMouse.move(pos); 
        }
        Timer {
            running: true;
            interval: 6500;
            onTriggered: virtualMouse.pick();
        }
        property int mouseX : (tileX+0.5)*c.tileSize;
        property int mouseY : (tileY+0.5)*c.tileSize;
        property bool containsMouse : true;
        signal pressed();

        Image {
            visible: mouseView.mouse == virtualMouse; 
            x: virtualMouse.mouseX; y: virtualMouse.mouseY;
            source: 'mouse.svg'
        }
    }

    MouseArea {
        id: realMouse;
        anchors.fill: parent;
        hoverEnabled: true;
    }
}
