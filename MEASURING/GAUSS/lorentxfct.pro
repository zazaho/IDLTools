function lorentzfct,x,p

;berekent de waarde van een lorentzfunctie
; f(x)=p1 p3^2/4/[(x-p2)^2+p3^2/(4*!PI)
; met parameters p=[p1,p2,p3]
;p1 piek diepte
;p2 piek positie
;p3 dempingscte

p(0)=abs(p(0))

y=p(0)*p(2)^2/4/[(x-p(1))^2+p(2)^2/4]

yr=y*p(0)/max(y)

return,yr
end


function doublelorentzfct,x,p

;berekent de waarde van een lorentzfunctie
;f=f1+f2 
; f1(x)=p1 p3^2/4/[(x-p2)^2+p3^2/(4*!PI)
; met parameters p=[p1,p2,p3]
;p1 piek diepte
;p2 piek positie
;p3 dempingscte

p(0)=abs(p(0))
p(3)=abs(p(3))

y1=p(0)*p(2)^2/4/[(x-p(1))^2+p(2)^2/4]
y1r=y1*p(0)/max(y1)

y2=p(3)*p(5)^2/4/[(x-p(4))^2+p(5)^2/4]
y2r=y2*p(3)/max(y2)

yr=y1r+y2r

return,yr
end

function trilorentzfct,x,p

;berekent de waarde van een lorentzfunctie
;f=f1+f2+f3 
; f1(x)=p1 p3^2/4/[(x-p2)^2+p3^2/(4*!PI)
; met parameters p=[p1,p2,p3]
;p1 piek diepte
;p2 piek positie
;p3 dempingscte


p(0)=abs(p(0))
p(3)=abs(p(3))
p(6)=abs(p(6))


y1=p(0)*p(2)^2/4/[(x-p(1))^2+p(2)^2/4]
y1r=y1*p(0)/max(y1)

y2=p(3)*p(5)^2/4/[(x-p(4))^2+p(5)^2/4]
y2r=y2*p(3)/max(y2)


y3=p(6)*p(8)^2/4/[(x-p(7))^2+p(8)^2/4]
y3r=y3*p(6)/max(y3)

yr=y1r+y2r+y3   

return,yr
end


function quatlorentzfct,x,p

;berekent de waarde van een lorentzfunctie
;f=f1+f2+f3+f4 
; f1(x)=p1 p3^2/4/[(x-p2)^2+p3^2/(4*!PI)
; met parameters p=[p1,p2,p3,p4]
;p1 piek diepte
;p2 piek positie
;p3 dempingscte


p(0)=abs(p(0))
p(3)=abs(p(3))
p(6)=abs(p(6))
p(9)=abs(p(9))


y1=p(0)*p(2)^2/4/[(x-p(1))^2+p(2)^2/4]
y1r=y1*p(0)/max(y1)

y2=p(3)*p(5)^2/4/[(x-p(4))^2+p(5)^2/4]
y2r=y2*p(3)/max(y2)


y3=p(6)*p(8)^2/4/[(x-p(7))^2+p(8)^2/4]
y3r=y3*p(6)/max(y3)

y4=p(9)*p(11)^2/4/[(x-p(10))^2+p(11)^2/4]
y4r=y4*p(9)/max(y3)

yr=y1r+y2r+y3r+y4r   

return,yr
end
