import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Layouts 1.0
import HsQML.Model 1.0

Window {
    visible: true;
    width: 900; height: 800;

    RowLayout {
        anchors.fill: parent;

        Item {
            Layout.fillWidth: true;
            Layout.fillHeight: true;
            Board {
                id: board;
                anchors.fill: parent;
            }
            Rectangle {
                width: 1.2*btnText.width;
                height: 1.2*btnText.height;
                anchors.centerIn: parent;
                color: 'green';
                radius: 0.1*btnText.height;
                enabled: board.gameOver;
                opacity: board.gameOver ?
                    (btnArea.containsMouse ? 1 : 0.5) : 0;
                Behavior on opacity {
                    NumberAnimation { duration: 500; }
                }
                MouseArea {
                    id: btnArea;
                    anchors.fill: parent;
                    hoverEnabled: true;
                    onClicked: board.start(false);
                    cursorShape: Qt.PointingHandCursor;
                }
                Text {
                    id: btnText;
                    anchors.centerIn: parent;
                    color: 'white';
                    font.pixelSize: 0.1*board.height;
                    text: '(Re)Start';
                }
            }
        }
        ListView {
            id: tileList;
            model: AutoListModel {
                mode: AutoListModel.ByKey;
                source: board.tileSource.topN(tileList.height/100);
                equalityTest: function(a, b) {return a.tile == b.tile;}
                keyFunction: function(x) {return x.idx;}
            }
            delegate: Item {
                width: 100; height: 100; 
                Repeater {
                    anchors.fill: parent;
                    model: sparePart(modelData.tile);
                    Plumb {
                        anchors.fill: parent;
                        entryA: modelData.entryA;
                        exitA: modelData.exitA;
                        volume: 0;
                    }
                }
            }
            displaced: Transition {
                NumberAnimation { properties: "x,y"; duration: 1000; }
                NumberAnimation { properties: "scale"; duration: 1000; to: 1; }
            }
            add: Transition {
                NumberAnimation {
                    properties: "scale"; duration: 1000; from: 0; to: 1;
                }
            }
            remove: Transition {
                NumberAnimation {
                    properties: "scale"; duration: 1000; to: 0;
                }
            }
            Layout.fillHeight: true;
            width: 100;
        }
    }
}
