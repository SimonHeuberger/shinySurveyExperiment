console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");Shiny.addCustomMessageHandler("envBanditTwoArms",function(a){a=JSON.parse(JSON.stringify(a));enOne=a.enOne;enTwo=a.enTwo;nTrials=a.nTrials;gameNr=[a.game];nDigits=[a.nDigits]});
function endGame(a,b,d,f,e,g){Shiny.onInputChange("selection",a);Shiny.onInputChange("outcome",b);Shiny.onInputChange("outcomeCum",d);Shiny.onInputChange("respTime",f);Shiny.onInputChange("trial",e);Shiny.onInputChange("gameNr",g)}function myRound(a,b){var d=Math.pow(10,b);return Math.round(a*d)/d}function newGame(){ind=[];selection=[];outcome=[];outcomeCum=[];t=[Date.now()];respTime=[];trial=[];clickEnabled=[1]}function add(a,b){return a+b}
function updateBanditTwoArms(a,b,d,f,e,g,c,r,q,k,l,m,n,u,v,h,p,w){c.length<m&&1===p[p.length-1]&&(g=Date.now(),p.push(0),u.push((g-h[h.length-1])/1E3),h.push(g),a=document.getElementById(a),b=document.getElementById(b),d=document.getElementById(d),h=document.getElementById(f),f=0===e[c.length]?"#BEBEBE":0>e[c.length]?"#FF6A6A":"#00CD00",a.innerHTML=e[c.length],a.style.color=f,l[l.lenth]!=r&&(b.innerHTML=" "),l.push(r),q.push(e[c.length]),v.push(c.length+1),h.innerHTML=m-(c.length+1),k.push(q.reduce(add,
0)),c.push(1),n.length<m&&n.push(n[0]),d.innerHTML=myRound(k[k.length-1],w),c.length===m&&endGame(l,q,k,u,v,n),setTimeout(function(){p.push(1)},500))};