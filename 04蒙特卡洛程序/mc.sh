#!/bin/bash
clear all
rm -rf *~
for inputfile in data.*
do
echo $i
### Derive output names from input name:
base_name="${inputfile}"

b="${base_name}"
n=300
gnuplot -persist  << EOF
#set  xlabel "x=52"      font "arial,18"       ##设置x坐标的标签###
#set  ylabel "y=52" font "arial,18"       ##设置y坐标的标签###
#set label "sin(x)" at 0.5,0.5               ##在坐标(0.5,0.5)处加入字符串‟sin(x)‟
#set arrow from 0.0,0.0 to 0.6,0.8 lt 3 lw 2 ##这个箭头颜色类型为 3, 1: 红色 2：绿色 3:蓝色 4：粉红 5：淡蓝 6：棕色 7：米蓝   8：橙色线宽类型为 2。在lt之前加入关键字nohead可以消除箭头变直线
#set xtics 1.0                               ##x 轴的主刻度的宽度为 1.0
#set ytics 1.0                               ##y 轴的主刻度的宽度为 1.0
#set mxtics 5                                ##x轴上每个主刻度中画5个分刻度 
#set mytics 5                                ##y轴上每个主刻度中画5个分刻度
####设定坐标轴范围#####
set xrange[-$n:$n]
set yrange[-$n:$n]
####关于图例的位置######  
set  term pdfcairo lw 2 font "arial,18"
set output "precipitation.pdf"
plot "$base_name"   u 2:3 w p pointtype 7  pointsize 0.3 linetype 0

#set key left #放在左边
#set key bottom ###放在下边,只有这一个选项;默认在上边
#set key outside ###放在外边,但只能在右面的外边
#set key 0.5,0.6###将图例放在 0.5,0.6 的位置处
#####图的尺寸##########
#set size 0.5,0.5  ###长宽均为默认宽度的一半,建议用这个取值
set size square   ###使图形是方的
######曲线拟合##################
#y(x) = a*x**2 + b*x +c              #####求幂的操作符是 **。第二条命令就是传说中的 fit 了。其中的 via 是关键字， 后面跟着需要拟合的变量列表。
#fit y(x)  "log.log" via a,b ,c
#plot "log.log" w l lw 2 title "400K" , y(x)  w l lw 3 title "300K" 
#######输出#####################
set output
set term pngcairo lw 2 font "arial,14"
set output "$b.png"
replot 
set output
set term wxt
EOF
done
 convert -delay 50 *.png -loop 0 animated.gif
rm -rf  *pdf 
mkdir temp
mv data*   temp
