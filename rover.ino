#include <SoftwareSerial.h>   //Software Serial Port

#undef DEBUG_ENABLED

//Standard PWM DC control
int E1 = 6;     //M1 Speed Control
int E2 = 5;     //M2 Speed Control
int M1 = 7;    //M1 Direction Control
int M2 = 4;    //M1 Direction Control

#define RxD 2 //if shield has pin BT_RX set to 1
#define TxD 3 //if shield has pin BT_TX set to 0

SoftwareSerial bluetooth(RxD,TxD);
// #define bluetooth Serial

void sendBluetoothCommand(char command[]) {
    bool needO = true;
    bool notOK = true;
    bluetooth.print(command);
    bluetooth.flush();
    Serial.print(command);
    delay(2000);

    while (bluetooth.available() == 0) { };

    Serial.print("r:(");
    char val = bluetooth.read();

    while (val != -1) {
        if (needO) {
            if (val == 'O')
                needO = false;
        } else {
            if (val == 'K') {
         //       Serial.print ("GOTOK");
                notOK = false;
            }
            else
                needO = true;
        }

        switch(val) {
            case '\n':
                Serial.print ("\\n");
                break;
            case '\r':
                Serial.print ("\\r");
                break;
            default:
                Serial.print(val);
        }

        while (notOK && bluetooth.available() == 0) { };
        val = bluetooth.read();
    }
    Serial.println(")");
}

void stop(void)                    //Stop
{
  digitalWrite(E1,LOW);
  digitalWrite(E2,LOW);
}

void advance(char a, char b)          //Move forward
{
  analogWrite (E1,a);      //PWM Speed Control
  digitalWrite(M1,HIGH);
  analogWrite (E2,b);
  digitalWrite(M2,HIGH);
}

void back_off (char a, char b)          //Move backward
{
  analogWrite (E1,a);
  digitalWrite(M1,LOW);
  analogWrite (E2,b);
  digitalWrite(M2,LOW);
}

void turn_R (char a, char b)             //Turn Right
{
  analogWrite (E1,a);
  digitalWrite(M1,HIGH);
  analogWrite (E2,b);
  digitalWrite(M2,LOW);
}

void turn_L (char a,char b)             //Turn Left
{
  analogWrite (E1,a);
  digitalWrite(M1,LOW);
  analogWrite (E2,b);
  digitalWrite(M2,HIGH);
}

void motor (uint8_t m1_direction, uint8_t m1_speed, uint8_t m2_direction, uint8_t m2_speed)             //Turn Left
{
  analogWrite (E1,m1_speed);
  digitalWrite(M1,m1_direction);
  analogWrite (E2,m2_speed);
  digitalWrite(M2,m2_direction);
}

void setup()
{
    // setup motor
    int i;
    for(i=4; i<=7; i++)
        pinMode(i, OUTPUT);

    pinMode(A1, INPUT);
    delay(1000);

    // setup Bluetooth serial connection
    pinMode(RxD, INPUT);
    pinMode(TxD, OUTPUT);

#if 0
    Serial.begin(38400);                   // initialize Serial
    Serial.print("\r\n+STBD=9600\r\n");   // set the baud rate
    Serial.end();

    delay(2000);
#endif

    Serial.begin(9600);                    // initialize Serial
    bluetooth.begin(9600);                    // initialize Serial

    sendBluetoothCommand("\r\n+STBD=9600\r\n");     // set the Serial work in slave mode
    sendBluetoothCommand("\r\n+STWMOD=0\r\n");     // set the Serial work in slave mode
    sendBluetoothCommand("\r\n+STNA=Rover\r\n");   // set the Serial name as "Rover"
//    Serial.print("\r\n+STPIN=0000\r\n"); // Set SLAVE pincode "0000"
    sendBluetoothCommand("\r\n+STOAUT=1\r\n");     // Permit Paired device to connect me
    sendBluetoothCommand("\r\n+STAUTO=0\r\n");     // Auto-connection should be forbidden here
    sendBluetoothCommand("\r\n+INQ=1\r\n");        // make the slave Serial inquirable
}

void loop ()
{
    if (digitalRead(A1) && bluetooth.available())
    {
        char val = (char)bluetooth.read();
        while(val != -1) {
            switch (val) {
            case 'f':
//                Serial.println("forward.");
                advance  (255, 255);
                break;
            case 'b':
//                Serial.println("backward.");
                back_off (255, 255);
                break;
            case 'l':
//                Serial.println("left.");
                turn_L   (255, 255);
                break;
            case 'r':
//                Serial.println("right.");
                turn_R   (255, 255);
                break;
            case 's':
//                Serial.println("stop.");
                stop();
                break;
            case 'm':
            {
                int m1 = 0;
                int m2 = 0;

                uint8_t m1_speed;
                uint8_t m2_speed;
                uint8_t m1_dir;
                uint8_t m2_dir;

                m1 = bluetooth.parseInt();
                m2 = bluetooth.parseInt();

                m1_speed = abs(m1);
                if (m1 >= 0) m1_dir = HIGH; else m1_dir = LOW;
                m2_speed = abs(m2);
                if (m2 >= 0) m2_dir = HIGH; else m2_dir = LOW;
                Serial.println(m1);
                Serial.println(m2);
                motor (m1_dir, m1_speed, m2_dir, m2_speed);
                break;
            }
            case '\r':
            case '\n':
                break;
            default:
                Serial.print("unknown command: ");
                Serial.println(val);
            }
            val = bluetooth.read();
        }
    }
}
