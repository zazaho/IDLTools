FUNCTION timmi_fitshift_fitfunction,p
  COMMON COMMON_timmi_fitshift,img1,img2
  ;; We have 4 independant fit parameters:
  ;; p[0] shift in the x direction
  ;; p[1] shift in the y direction
  ;; p[2] offset level
  ;; p[3] scaling factor
  ;; Shift the image and subtract it from the comparison image(img1)

  diff = img1-(p[3]*(timmi_shift(img2,p[0],p[1]))+p[2])
  ;; timmi_shift is een functie om een image te shiften met een
  ;; subpixel resolutie
  return,total(diff^2d0)

END

;; Voorbeeld kaal gemaakt om een image te alignen met een ander image  

FUNCTION timmi_fitshift,i1,i2
  
  COMMON COMMON_timmi_fitshift,img1,img2
  
  img1=i1
  img2=i2
  
  p = amoeba(1d-6,FUNCTION_NAME='timmi_fitshift_fitfunction', $
                  P0=[0d0,0d0,0d0,1d0],SCALE=[10d0,10d0,median(img2),2d0])
  ;; geef de gevonden x en y shift terug
  return,[p[0],p[1]]
END
