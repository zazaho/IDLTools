;; routine to make a 2D fractal distribution.
;; The procedure to follow:
;;
;; Subdivide the space in a number of SubBoxes (SubDivision1D^2).
;; Randomly select a fixed fraction of these SubBoxes (Children)
;;   Nran = NDiv^2 * Fraction
;; Inside the Children we make new subdivisions
;; Repeat this until total number of requested iterations
;; (NIterations) is reached


;; Helper routine
function GenerateFractal2D_subdivide, $
   Coord, $
   Iter=Iter, $
   NDiv=NDiv, $
   Nran=Nran, $
   PerturbationScale=PerturbationScale, $
   LastIter=LastIter

  ScaleFactor = (1d0/NDiv)^double(Iter)
  
  ;; make a empty list to hold the generated children at this level
  ChildList=LIST()

  ;; We generate the list of children
  for cc=1L,Nran do begin
     ;; this selects a random subbox (X,Y)
     ChildCoord = Coord + ScaleFactor*FLOOR(RANDOMU(seed,/DOUBLE,2)*NDiv)

     ;; make sure we do not have a duplicate child
     while ChildList.Where(ChildCoord) ne !NULL do begin
        ChildCoord = Coord + ScaleFactor*FLOOR(RANDOMU(seed,/DOUBLE,2)*NDiv)
     endwhile
     ChildList=ChildList+LIST(ChildCoord)
  endfor

  ;; With a small pertubation?
  if PerturbationScale ne 0d0 then begin
     for ll=0,n_elements(ChildList)-1 do begin
        ChildList[ll] = ChildList[ll] + PerturbationScale*ScaleFactor*RANDOMN(seed,/DOUBLE,2)
     endfor
  endif

  ;; make a empty list to hold the final generated children
  ChildrenList=LIST()
  
  ;; do we want to recurse further?
  if Iter lt LastIter then begin
     foreach ChildCoord, ChildList do begin
        ChildrenList = ChildrenList+GenerateFractal2D_subdivide(ChildCoord,Iter=Iter+1,NDiv=NDiv,Nran=Nran,LastIter=LastIter,Pert=PerturbationScale)
     endforeach
  endif else begin
     ;; if this is the end of the recursion add the coordinates to
     ;; the list
     ChildrenList = ChildrenList+ChildList
  endelse

  return,ChildrenList

end

;; Main routine
function GenerateFractal2D, $
   NStars=NStars, $
   NIterations=NIterations, $
   NDiv=NDiv, $
   PerturbationScale=PerturbationScale, $
   FractalDimension=FractalDimension, $
   FinalNStars=FinalNStars, $
   InfoHeader=InfoHeader
  
  default,NDiv,3d0
  default,FractalDimension,1d0 ;; 3/9
  
  ;; Number of subsquares to be selected in each iteration
  Nran=round(Ndiv^FractalDimension)
  message,/info,'Effective Fractal dimension after rounding: '+string(format="(F)",alog(Nran)/alog(NDiv))
  message,/info,'Fraction of subsquares to be selected: '+string(format="(F)",double(Nran)/(NDiv^2))

  if Nran le 1 then begin
     message,/info,'Number of children is calculated to be less than one!'
     return,-1
  endif

  if Nran ge NDiv^2 then begin
     message,/info,'Number of children is calculated to be all subboxes!'
     return,-1
  endif
  
  ;; target number of stars to generate
  default,NStars,50d3
  default,NIterations,ceil(alog(NStars)/alog(Nran))

  ;; the number of stars that is really calculated
  RealNStars = Nran^NIterations
  
  ;; this allows to randomly select a fixed number of stars from the
  ;; final catalog
  default,FinalNStars,RealNStars

  ;; should not be larger the RealNStars
  FinalNStars = (FinalNStars<RealNstars)

  message,/info,'Will calculate the positions of '+string(format="(I)",Nran^NIterations)+' stars'
  
  default,PerturbationScale,0d0
  
  ;; create the children (recursive!)
  ;; this will randomly select children in the box (0..1,0..1,0..1)
  ChildrenList=GenerateFractal2D_subdivide([0d0,0d0],Iter=1,NDiv=NDiv,Nran=Nran,LastIter=NIterations,PerturbationScale=PerturbationScale)

  ;; if we want a smaller number of stars in the final catalog then
  ;; generated, keep the requires number of stars after shuffling the
  ;; index of the points that have been generated
  if FinalNStars lt RealNStars then begin
     ChildrenList = ChildrenList[(Shuffle(lindgen(RealNstars)))[0:FinalNstars-1]]
  endif
  
  ;; Convert the list to a simple [2,*] array with each row a
  ;; coordinate triple
  Children=ChildrenList.toArray(/transpose)

  ;; Fill the InfoHeader so that the returning function knows what we
  ;; have calculated exactly
  
  InfoHeader  = "# #"+string(10B)
  InfoHeader += "# Requested fractal dimesion: "+n2s(FractalDimension)+string(10B)
  InfoHeader += "# Effective fractal dimension: "+n2s(alog(Nran)/alog(NDiv))+string(10B)
  InfoHeader += "# 1D division factor per iteration: "+n2s(NDiv)+string(10B)
  InfoHeader += "# Number of children per generation: "+n2s(NRan)+string(10B)
  InfoHeader += "# Number of iterations: "+n2s(NIterations)+string(10B)
  InfoHeader += "# Number of generated stars: "+n2s(RealNStars)+string(10B)
  InfoHeader += "# Number of kept stars: "+n2s(FinalNStars)+string(10B)
  InfoHeader += "# Perturbation scale: "+n2s(PerturbationScale)+string(10B)
  InfoHeader += "# #"

  return,Children

end

;; Standard example run and visualisation
FractalDimension=1.5d0
PerturbationScale=0.1d0
NStars=5150d0

Distribution2D = GenerateFractal2D(NStars=NStars,FractalDimension=FractalDimension,Pert=PerturbationScale)
s=plot(Distribution2d,linestyle="none",symbol="dot")

end
