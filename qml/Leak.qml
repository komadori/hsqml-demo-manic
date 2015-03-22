import QtQuick 2.0
import QtGraphicalEffects 1.0

Item {
    id: c;
    anchors.fill: parent;

    property double tileX : 0;
    property double tileY : 0;
    property double tileSize : 100;
    property int exitBearing : 0;
    property color colour : 'green';
    property bool trigger : false;

    onTriggerChanged: if (trigger) {sizeAnim.start();}

    RadialGradient {
        id: grad;
        anchors.fill: parent;
        property double cx: {
            var h=0;
            switch (c.exitBearing) {
            case 0: h=0.5; break; case 90: h=0.9; break;
            case 180: h=0.5; break; case 270: h=0.1; break;
            }
            return (c.tileX+h)*c.tileSize;
        }
        horizontalOffset: cx-c.width/2;
        property double cy: {
            var v=0;
            switch (c.exitBearing) {
            case 0: v=0.1; break; case 90: v=0.5; break;
            case 180: v=0.9; break; case 270: v=0.5; break;
            }
            return (c.tileY+v)*c.tileSize;
        }
        verticalOffset: cy-c.height/2;
        horizontalRadius: size*1.5*c.tileSize;
        verticalRadius: size*1.5*tileSize;
        gradient: Gradient {
            GradientStop { position: 0.9; color: c.colour; }
            GradientStop { position: 1.0; color: Qt.rgba(0,0,0,0); }
        }

        property double size : 0;
        NumberAnimation on size {
            id: sizeAnim;
            from: 0; to: 1;
            duration: 3000;
            easing.type: Easing.OutQuad;
            running: false;
        }
    }
}
