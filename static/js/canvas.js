var ctx = document.getElementsByTagName('canvas')[0].getContext('2d');

var skyColor = '#33f';
var earthColor = '#992';

var majorWidth = 100;
var minorWidth = 60;
var pixelsPerDeg = 10;

var roll = 0;// 20/180*Math.PI;
var pitch = 20/180*Math.PI;
var altitude = -100;
var speed = 53;

var zeroWidth = 200;
var zeroGap = 20;
var radialLimit = 60;
var tickRadius = 10;
var radialRadius = 200;



function drawHorizon() {
    
    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);

    ctx.rotate(-roll);
    var pitchPixels = pitch/(Math.PI*2) * 360 * pixelsPerDeg;
    ctx.translate(0, pitchPixels);

    ctx.fillStyle = skyColor;
    ctx.fillRect(-10000, -10000, 20000, 10000);
    ctx.fillStyle = earthColor;
    ctx.fillRect(-10000, 0, 20000, 10000);

    ctx.strokeStyle = '#fff';
    ctx.fillStyle = 'white';
    ctx.lineWidth = 2;

    ctx.beginPath();  
    ctx.moveTo(-10000, 0);  
    ctx.lineTo(20000, 0);
    ctx.stroke(); 

    ctx.beginPath();
    ctx.arc(0, -pitchPixels, radialRadius, 
            0, Math.PI*2,
            false /* anti-clockwise */);
    ctx.closePath();
    ctx.clip();

    ctx.beginPath();  
    for (var i = -18; i <= 18; ++i) {
        var pitchAngle = i/2*10;
        if (i != 0) {
            if (i % 2 == 0) {
                ctx.moveTo(-majorWidth/2, -pixelsPerDeg*pitchAngle);  
                ctx.lineTo(+majorWidth/2, -pixelsPerDeg*pitchAngle);

                ctx.fillText(pitchAngle, -majorWidth/2 - 20, -pixelsPerDeg*10/2*i);
                ctx.fillText(pitchAngle, majorWidth/2 + 10, -pixelsPerDeg*10/2*i);

            } else {
                ctx.moveTo(-minorWidth/2, -pixelsPerDeg*pitchAngle);  
                ctx.lineTo(+minorWidth/2, -pixelsPerDeg*pitchAngle);
            }
        }
    }
    ctx.closePath();
    ctx.stroke();

    
    
    ctx.restore();
}


function drawZero() {
    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);

    ctx.strokeStyle = 'yellow';
    ctx.lineWidth = 2;

    ctx.beginPath();  
    ctx.moveTo(-zeroWidth/2, 0);  
    ctx.lineTo(-zeroGap/2, 0);
   ctx.moveTo(+zeroWidth/2, 0);  
    ctx.lineTo(+zeroGap/2, 0);

    ctx.moveTo(-zeroGap/2, zeroGap/2);
    ctx.lineTo(0, 0);
    ctx.lineTo(+zeroGap/2, zeroGap/2);

    ctx.stroke(); 

    // The radial roll indicator
    ctx.beginPath();
    ctx.arc(0, 0, radialRadius, 
            -Math.PI/2 - Math.PI*radialLimit/180, 
            -Math.PI/2 + Math.PI*radialLimit/180, 
            false /* anti-clockwise */);
    ctx.stroke();

    for (var i = -4; i <= 4; ++i) {
        ctx.moveTo((radialRadius - tickRadius)*Math.cos(-Math.PI/2 + i*15/180*Math.PI), 
                   (radialRadius - tickRadius)*Math.sin(-Math.PI/2 + i*15/180*Math.PI));
        ctx.lineTo(radialRadius*Math.cos(-Math.PI/2 + i*15/180*Math.PI), 
                   radialRadius*Math.sin(-Math.PI/2 + i*15/180*Math.PI));
    }
    ctx.stroke();

    ctx.restore();
}

function drawRoll() {
    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    ctx.rotate(-roll);

    ctx.fillStyle = 'white';
    ctx.lineWidth = 2;

    ctx.beginPath();  
    ctx.moveTo(0, -radialRadius);  
    ctx.lineTo(-5, -radialRadius + 10);
    ctx.lineTo(+5, -radialRadius + 10);  
    ctx.closePath();
    ctx.fill();
    
    var readableRollAngle = Math.round(roll/Math.PI/2*360) % 360;
    if (readableRollAngle > 180) {
        readableRollAngle = readableRollAngle - 360;
    }

    ctx.fillRect(-20, -radialRadius + 9,
                 40, 16);
    
    ctx.font = '12px Arial';
    ctx.fillStyle = 'black';
    ctx.fillText(readableRollAngle, -7, -radialRadius + 22);

    ctx.restore();
}

var speedIndicatorHeight = 250;
var speedIndicatorWidth = 60;
var zeroPadding = 100;
var speedAltOpacity = 0.2;
var pixelsPer10Kmph = 50;
var minorTicksPer10Kmph = 5;

var speedWarningWidth = 10;
var yellowBoundarySpeed = 100;
var redBoundarySpeed = 130;

function drawSpeed() {
    
    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    ctx.translate(-zeroWidth/2 - zeroPadding - speedIndicatorWidth, 0);

    ctx.fillStyle = 'rgba(0,0,0,' + speedAltOpacity + ')';
    ctx.strokeStyle = 'white';
    ctx.lineWidth = 2;
    
    ctx.strokeRect(0, -speedIndicatorHeight/2, speedIndicatorWidth, speedIndicatorHeight);
    ctx.fillRect(0, -speedIndicatorHeight/2, speedIndicatorWidth, speedIndicatorHeight);

    ctx.restore();

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    ctx.translate(-zeroWidth/2 - zeroPadding - speedIndicatorWidth, 0);

    ctx.rect(0, -speedIndicatorHeight/2, speedIndicatorWidth, speedIndicatorHeight);
    ctx.clip();

    var yellowBoundaryY = -(-speed + yellowBoundarySpeed)/10*pixelsPer10Kmph;
    var redBoundaryY = -(-speed + redBoundarySpeed)/10*pixelsPer10Kmph;

    ctx.fillStyle = 'yellow';
    ctx.fillRect(speedIndicatorWidth - speedWarningWidth, yellowBoundaryY, 
                 speedWarningWidth, redBoundaryY - yellowBoundaryY);

    ctx.fillStyle = 'red';
    ctx.fillRect(speedIndicatorWidth - speedWarningWidth, redBoundaryY, 
                 speedWarningWidth, -speedIndicatorHeight/2 - redBoundaryY );

    ctx.fillStyle = 'green';
    ctx.fillRect(speedIndicatorWidth - speedWarningWidth, yellowBoundaryY, 
                 speedWarningWidth, +speedIndicatorHeight/2 - yellowBoundaryY );

    var yOffset = speed/10*pixelsPer10Kmph;

    // The unclipped ticks to be rendered. We render 100kmph either side of the
    // center to be safe
    var from = -Math.floor(speed / 10)  - 10;
    var to = Math.ceil(speed / 10)  + 10;

    for (var i = from; i < to; ++i) {

        ctx.moveTo(speedIndicatorWidth - speedWarningWidth, -i*pixelsPer10Kmph + yOffset);
        ctx.lineTo(speedIndicatorWidth - speedWarningWidth - majorTickWidth, -i*pixelsPer10Kmph + yOffset);

        for (var j = 1; j < minorTicksPer10Kmph; ++j) {
            ctx.moveTo(speedIndicatorWidth - speedWarningWidth, -i*pixelsPer10Kmph - j*pixelsPer10Kmph/minorTicksPer10Kmph + yOffset);
            ctx.lineTo(speedIndicatorWidth - speedWarningWidth - minorTickWidth, -i*pixelsPer10Kmph - j*pixelsPer10Kmph/minorTicksPer10Kmph + yOffset);
        }
        
        ctx.font = '12px Arial';
        ctx.fillStyle = 'white';
        ctx.fillText(i*10, 20, -i*pixelsPer10Kmph + yOffset + 4);
    }
    ctx.strokeStyle = 'white';
    ctx.lineWidth = 2;
    ctx.stroke();

    ctx.beginPath();  
    ctx.moveTo(speedIndicatorWidth - speedWarningWidth - minorTickWidth, 0);
    ctx.lineTo(speedIndicatorWidth - speedWarningWidth - minorTickWidth*2, -5);
    ctx.lineTo(speedIndicatorWidth - speedWarningWidth - minorTickWidth*2, -10);
    ctx.lineTo(0, -10);  
    ctx.lineTo(0, 10);  
    ctx.lineTo(speedIndicatorWidth - speedWarningWidth - minorTickWidth*2, 10);
    ctx.lineTo(speedIndicatorWidth - speedWarningWidth - minorTickWidth*2, 5);
    ctx.closePath();

    ctx.fill();
    ctx.strokeStyle = 'black';
    ctx.fillStyle = 'black';
    ctx.fillText(speed, 15, 4.5, altIndicatorHeight);

    ctx.restore();

}



var altIndicatorHeight = 250;
var altIndicatorWidth = 50;
var majorTickWidth = 10;
var minorTickWidth = 5;
var pixelsPer100Ft = 50;
var minorTicksPer100Ft = 5;

function drawAltitude() {

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    ctx.translate(zeroWidth/2 + zeroPadding, 0);

    ctx.fillStyle = 'rgba(0,0,0,' + speedAltOpacity + ')';
    ctx.strokeStyle = 'white';
    ctx.lineWidth = 2;
    
    ctx.fillRect(0, -altIndicatorHeight/2, altIndicatorWidth, altIndicatorHeight);
    ctx.strokeRect(0, -altIndicatorHeight/2, altIndicatorWidth, altIndicatorHeight);

    ctx.restore();

    ctx.save();

    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    ctx.translate(zeroWidth/2 + zeroPadding, 0);

    ctx.rect(0, -altIndicatorHeight/2, altIndicatorWidth, altIndicatorHeight);
    ctx.clip();

    var yOffset = altitude/100*pixelsPer100Ft;

    // The unclipped ticks to be rendered. We render 500ft either side of the
    // center to be safe
    var from = Math.floor(altitude / 100)  - 5;
    var to = Math.ceil(altitude / 100)  + 5;

    for (var i = from; i < to; ++i) {
        ctx.moveTo(0, -i*pixelsPer100Ft + yOffset);
        ctx.lineTo(majorTickWidth, -i*pixelsPer100Ft + yOffset);

        for (var j = 1; j < minorTicksPer100Ft; ++j) {
            ctx.moveTo(0, -i*pixelsPer100Ft - j*pixelsPer100Ft/minorTicksPer100Ft + yOffset);
            ctx.lineTo(minorTickWidth, -i*pixelsPer100Ft - j*pixelsPer100Ft/minorTicksPer100Ft + yOffset);
        }
        
        ctx.font = '12px Arial';
        ctx.fillStyle = 'white';
        ctx.fillText(i*100, 15, -i*pixelsPer100Ft + yOffset + 4);
    }
    ctx.strokeStyle = 'white';
    ctx.lineWidth = 2;
    ctx.stroke();

    ctx.restore();

    ctx.save();
    ctx.translate(ctx.canvas.width/2, ctx.canvas.height/2);
    ctx.translate(zeroWidth/2 + zeroPadding, 0);

    ctx.strokeStyle = 'white';
    ctx.lineWidth = 2;
    
    ctx.font = '12px Arial';
    ctx.fillStyle = 'white';
    ctx.fillOpacity = 1.0;

    ctx.beginPath();  
    ctx.moveTo(minorTickWidth, 0);
    ctx.lineTo(minorTickWidth*2, -5);
    ctx.lineTo(minorTickWidth*2, -10);
    ctx.lineTo(altIndicatorWidth, -10);  
    ctx.lineTo(altIndicatorWidth, 10);  
    ctx.lineTo(minorTickWidth*2, 10);
    ctx.lineTo(minorTickWidth*2, 5);
    ctx.closePath();

    ctx.fill();
    ctx.strokeStyle = 'black';
    ctx.fillStyle = 'black';
    ctx.fillText(altitude, 15, 4.5, altIndicatorHeight);

    ctx.restore();
}


var dp = Math.PI/300;
var dr = Math.PI/200;
var n = 0;
var animate = false;
function draw() {
   
    if (animate) {
        if ((dp > 0) && (pitch + dp > Math.PI/2)) {
            dp = -dp;
            roll = roll + Math.PI;
        } 
        if ((dp < 0) && (pitch + dp < -Math.PI/2)) {
            dp = -dp;
            roll = roll + Math.PI;
        }
        pitch = pitch + dp;
        roll = roll + dr;
        altitude = altitude + 1;
        speed = speed + 1;

        n = n + 1;
    }
    
    drawHorizon();
    drawZero();
    drawRoll();
    drawSpeed();
    drawAltitude();
}

var n = 0;
$(document).ready(function() {
    if (animate) {
        setInterval(draw, 1000/25);
    } else {
        draw();
    }
});