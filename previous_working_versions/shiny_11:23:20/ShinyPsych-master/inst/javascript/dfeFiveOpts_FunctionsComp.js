console.log("VALUABLE RESEARCH NEEDS VALUABLE DATA. PLEASE DON'T CHEAT! \nIf you do please answer in the questionnaire honestly that you did so\nthat we can make sure to not use your data.\nThank you!");Shiny.addCustomMessageHandler("envDfeFiveOpts",function(a){a=JSON.parse(JSON.stringify(a));enOne=a.enOne;enTwo=a.enTwo;enThree=a.enThree;enFour=a.enFour;enFive=a.enFive;gambleNr=[a.gambleNr];signalColors=a.signalColors});
function endDfeGame(a,f,b,c,d,e,n){Shiny.onInputChange("selected",a);Shiny.onInputChange("samples",f);Shiny.onInputChange("finalOutcome",n);Shiny.onInputChange("outcome",b);Shiny.onInputChange("respTime",c);Shiny.onInputChange("trial",d);Shiny.onInputChange("gambleNr",e)}function newDfeGame(){ind=[];selected=[];finalOutcome=[];samples=[];outcome=[];t=[Date.now()];respTime=[];trial=[];clickEnabled=[1];makeDecision=[!1]}function add(a,f){return a+f}
function enableDecisionFiveOpt(a,f,b,c,d,e){Shiny.onInputChange("toggleButton",2);f=document.getElementById(f);b=document.getElementById(b);c=document.getElementById(c);d=document.getElementById(d);e=document.getElementById(e);f.innerHTML=" ";b.innerHTML=" ";c.innerHTML=" ";d.innerHTML=" ";e.innerHTML=" ";a.push(!0)}
function updateDfeFiveOpts(a,f,b,c,d,e,n,B,C,D,g,p,u,k,q,v,l,r,h,z,w,x,y){if(1!=w[w.length-1]){if(1===h[h.length-1]){n=Date.now();h.push(0);v.push((n-r[r.length-1])/1E3);r.push(n);var m=document.getElementById(a);a=document.getElementById(f);b=document.getElementById(b);c=document.getElementById(c);d=document.getElementById(d);var A=1===z?0===e[g.length]?"#BEBEBE":0>e[g.length]?"#FF6A6A":"#00CD00":"#000000";m.innerHTML=" ";setTimeout(function(){m.innerHTML=e[g.length];m.style.color=A},100);k[k.lenth]!=
p&&(a.innerHTML=" ",b.innerHTML=" ",c.innerHTML=" ",d.innerHTML=" ");k.push(p);u.push(e[g.length]);l.push(g.length+1);g.push(1);1<l[l.length-1]&&q.push(q[0]);if(1===l[l.length-1])Shiny.onInputChange("toggleButton",1);setTimeout(function(){h.push(1)},500)}}else 1===h[h.length-1]&&(h.push(0),m=document.getElementById(a),a=document.getElementById(f),b=document.getElementById(b),c=document.getElementById(c),d=document.getElementById(d),m.innerHTML=" ",m.style.backgroundColor="#A9A9A9",k[k.lenth]!=p&&
(a.innerHTML=" ",b.innerHTML=" ",c.innerHTML=" ",d.innerHTML=" "),x.push(p),y.push(e[g.length]),g.push(1),setTimeout(function(){endDfeGame(x,k,u,v,l,q,y)},700))};