;; MANDELBROT RENDERER	    
	    
	    

(defun meq (z c)							;The basic iterative function
  (+ c (* z z)))
  
(defun mand (num-iter r i)						;The iterator... returns '*' if point is in the set, ' ' if not
  (do
   ((iter 0 (+ 1 iter))
    (z 0 (meq z (complex r i))))
   ((or (>= iter num-iter)
	(> (abs z) 2))
    (if (<= (abs z) 2)
	'*
      " " ))))
   
(defun print-array (input-array)					;Prints the array in kinda formatted columns
  (let ((x-dim (array-dimension input-array 0))
	(y-dim (array-dimension input-array 1)))
    (dotimes (y y-dim)
      (dotimes (x x-dim)
	(format t "~a" (aref input-array x (- y-dim (+ y 1)))))
      (format t "~%"))))

(defun map-array (a-index a-dim d-min d-max)				;Scales array index values to the complex plane window
  (let ((d-scale (/ (- d-max d-min) a-dim)))
    (+ (* d-scale a-index) d-min)))


(defun mandelbrot-render (num-iter x-dim y-dim r-min r-max i-min i-max)	;Brings everything together...syntax and examples given below
  (let ((mandelbrot (make-array (list x-dim y-dim))))
    (do ((x 0 (+ 1 x))
	 (r r-min (map-array x x-dim r-min r-max)))
    ((= x x-dim))
    (do ((y 0 (+ 1 y))
	 (i i-min (map-array y y-dim i-min i-max)))
	 ((= y y-dim))
	 (setf (aref mandelbrot x y)
	       (mand num-iter r i))))
	 
  (print-array mandelbrot)))



#|| 
Syntax:
(mandelbrot-render [number of iterations] [horizontal print array dimension] [vertical print array dimension] [complex plane real minimum] [complex '' real maximum] ['' imaginary mininum] ['' maximum])

*** Don't set your initial rendering of a window at more than 14 iterations, or else the calculations might get too intensive (something with fractions vs. floats) Instead, find your frame at 10 or 12 iterations, and then increase from there.
		   
Fun examples:
		   
(mandelbrot-render 14 80 36 -2 0.6 -1.25 1.25)
(mandelbrot-render 14 80 40 -1 0 0 1)
(mandelbrot-render 100 80 36 -0.7 -0.65 0.3 0.45)

(mandelbrot-render 100 149 300 -0.35  -0.0 0 1.35)






(defun prime-fac (n)							;An unrelated prime factorizer just for fun
  (let ((sq (isqrt n)))
    (do
     ((d 2 (1+ d)))
     ((or (>= d sq)
	  (zerop (mod n d)))
      (if (zerop (mod n d))
	  (cons d (prime-fac
		   (/ n d)))
	(if (/= n 1)
	    (list n)))))))
	
	
	


