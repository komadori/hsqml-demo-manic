import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Layouts 1.0
import HsQML.Model 1.0

Window {
    visible: true;
    width: 900; height: 800;

    RowLayout {
        anchors.fill: parent;

        Rectangle {
            Layout.fillWidth: true;
            Layout.fillHeight: true;
            color: 'cyan';
            Board {
                id: board;
                anchors.fill: parent;
            }
        }
        ListView {
            model: AutoListModel {
                mode: AutoListModel.ByEquality;
                source: board.tileSource.topN(8);
            }
            delegate: Item {
                width: 100; height: 100; 
                Repeater {
                    anchors.fill: parent;
                    model: sparePart(modelData);
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
            }
            remove: Transition {
                NumberAnimation { properties: "scale"; duration: 1000; from: 1; to: 0; }
            }
            Layout.fillHeight: true;
            width: 100;
        }
    }
}
