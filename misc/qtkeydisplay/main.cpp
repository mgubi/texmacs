#include <QApplication>
#include <QWidget>
#include <QLabel>
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QKeySequence>

class KeyWidget : public QWidget {
public:
    KeyWidget(QWidget *parent = nullptr) : QWidget(parent) {
        QVBoxLayout *layout = new QVBoxLayout(this);
        
        infoLabel = new QLabel("Press any key...", this);
        infoLabel->setAlignment(Qt::AlignCenter);
        
        QFont font = infoLabel->font();
        font.setPointSize(24);
        font.setBold(true);
        infoLabel->setFont(font);

        layout->addWidget(infoLabel);
        setLayout(layout);
        
        setWindowTitle("Qt6 Key Event Example");
        resize(400, 300);

        setFocusPolicy(Qt::StrongFocus);
    }

protected:
    bool focusNextPrevChild(bool next) override {
        return false;
    }

    void keyPressEvent(QKeyEvent *event) override {
        int key = event->key();
        Qt::KeyboardModifiers modifiers = event->modifiers();

        bool isModifier = (key == Qt::Key_Control || key == Qt::Key_Shift || 
                           key == Qt::Key_Alt || key == Qt::Key_Meta);

        QString text;
        if (isModifier) {
            text = QKeySequence(key).toString();
        } else {
            text = QKeySequence(modifiers | key).toString();
        }

        if (key == Qt::Key_Backtab) {
             text = "HACK : Shift+Tab";
        }

        if (text.isEmpty()) {
            text = QString("Unknown Key (%1)").arg(key);
        }

        infoLabel->setText("Pressed: " + text);

        event->accept();
    }
    
private:
    QLabel *infoLabel;
};

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);
    KeyWidget window;
    window.show();
    return app.exec();
}