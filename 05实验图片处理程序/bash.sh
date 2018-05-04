#!/bin/bash
for ((i=2;i<=10;i++))
do
ifort -o strainstress maxstrain-stress.f90
cp bash.sh strainstress maxstrain-stress.f90 $i 
cd $i
rm -rf data

for ((i=1;i<=200;i++))
do
 echo $i
# cp *-$i.bmp $i.bmp
# convert *-$i.bmp -monochrome out.png
# convert 3.bmp -white-threshold 30% -monochrome out.bmp
 convert *-$i.jpg -white-threshold 10% -monochrome out.txt
 convert *-$i.jpg -white-threshold 10% -monochrome $i.png
 ./strainstress >>data
 rm -rf out.txt
done
cd ..
done
