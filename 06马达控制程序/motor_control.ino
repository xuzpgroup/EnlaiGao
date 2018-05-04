
/*
 Stepper Motor Control - one revolution

 This program drives a unipolar or bipolar stepper motor.
 The motor is attached to digital pins 8 - 11 of the Arduino.

 The motor should revolve one revolution in one direction, then
 one revolution in the other direction.



 Modified 30 Nov. 2016
 by Enlai Gao

 */

#include <Stepper.h>
unsigned long zhuanshu=526;

const int stepsPerRevolution = 200;  // change this to fit the number of steps per revolution
// for your motor

// initialize the stepper library on pins 8 through 11:
Stepper myStepper(stepsPerRevolution, 8, 9, 10, 11);

void setup() {
      delay(20000);
  // set the speed at 60 rpm:
  myStepper.setSpeed(100);
  // initialize the serial port:
  Serial.begin(9600);
}
unsigned long counter=0;


int i=0;
void loop() {
 counter=1;
 while (counter<=zhuanshu)
    {
   counter++;
   Serial.println("clockwise");
   myStepper.step(stepsPerRevolution);
     } 
 delay(2000);
 counter=1;
 while (counter<=zhuanshu)
    {
    counter++;
   Serial.println("clockwise");
   myStepper.step(-stepsPerRevolution);
     }
  delay(2000);
}

