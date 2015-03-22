import QtQuick 2.0

Grid {
    columns: 5;
    columnSpacing: 5;
    rowSpacing: 5;

    property var colours: ['lime','yellow','darkgoldenrod','blue'];
    property int colIndex: 0;

    property double vol: 0;
    SequentialAnimation on vol {
        NumberAnimation {
            from: 0; to: 0; duration: 1000;
        }
        NumberAnimation {
            from: 0; to: 1.1; duration: 5000;
        }
        NumberAnimation {
            from: 1.1; to: 1.1; duration: 1000;
        }
        ScriptAction {
            script: colIndex = (colIndex+1)%colours.length;
        }
        loops: Animation.Infinite;
    }

    Repeater {
        model: 25;

        Plumb {
            entryA: Math.floor(index / 5) * 90;
            exitA: (index % 5) * 90;
            colour: colours[colIndex];
            volume: vol;
        }
    }
}
