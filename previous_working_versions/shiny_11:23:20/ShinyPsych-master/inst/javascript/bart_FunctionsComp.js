console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");var pumps=0,maxPumps,popMax,actionTimes=[];Shiny.addCustomMessageHandler("maxPopHandler",function(a){popMax=a[0];drawBoundary=a[1]});Shiny.addCustomMessageHandler("maxPumpHandler",function(a){maxPumps=a});
function jsPump(a){actionTimes.push((new Date).getTime()-t);pumps+=1;pumps<=maxPumps?(document.getElementById("pumpCounter").innerHTML=pumps,jsRedrawBalloon(a)):(Shiny.onInputChange("popped",1),Shiny.onInputChange("pumps",pumps),Shiny.onInputChange("actionTimes",actionTimes))}function jsSaveBalloon(){actionTimes.push((new Date).getTime()-t);Shiny.onInputChange("actionTimes",actionTimes);Shiny.onInputChange("pumps",pumps)}
function jsNewBalloon(){pumps=0;Shiny.onInputChange("popped",0);actionTimes=[]}
function jsRedrawBalloon(a){balloonSize=pumps/popMax*95+5;ctx.clearRect(0,0,jsCanvas.width,jsCanvas.height);ctx.beginPath();ctx.arc(250,160,balloonSize,0,2*Math.PI);ctx.stroke();ctx.fillStyle=a;ctx.fill();ctx.fillStyle="black";ctx.font="24px Arial";"red"==a?(ctx.fillText("Popped at pump "+pumps,160,40),ctx.fillText("No points",360,170)):"green"==a&&(ctx.fillText("Saved at pump "+pumps,160,40),ctx.fillText("+ "+pumps+" points",360,170));drawBoundary&&(ctx.beginPath(),ctx.arc(250,160,100,0,2*Math.PI),
ctx.stroke())}function jsDelayButton(){document.getElementById("nextballoon").style.visibility="hidden";setTimeout(function(){document.getElementById("nextballoon").style.visibility="visible"},1E3)};
