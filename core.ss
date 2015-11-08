
#lang racket/base

(require "../fluxus-018/fluxus-modules.ss"
		 "../fluxus-018/input.ss"
		 "../fluxus-018/help.ss"
		 "../fluxus-018/camera.ss"
		 "../fluxus-018/building-blocks.ss"
		 "../fluxus-018/tasks.ss"
)

(provide
	m_autoresize ; no strecthed vis
	m_initaudio  ; init basic jack
	m_initmidi	 ; midi device
	m_midicc  ; midi get slider (id) value
	m_camlookat  ; glu lookat
	m_poltocar   ; polar to cartesian
	m_initosc
	m_getpos	 ;returns the world position of the grabbed primitive
	m_pripos
	m_gh
	m_gl
	m_help
	m_homedir
	m_filetime
	m_windowsize
)


(define (m_help)
	(print "m_autoresize m_initaudio m_initmidi m_midicc <cc number>")
	(newline)
	(print "m_camlookat  m_poltocar  m_initosc  m_getpos")
	(newline)
	(print "m_pripos     m_gh        m_gl       m_homedir")
	(newline)
	(print "m_filetime   m_filetime  m_windowsize")
	(newline)
)

(define (m_homedir path)
 (string-append (path->string (find-system-path 'home-dir)) "Documents/Fluxus" path)	
)


(define (m_filetime path)
	(file-or-directory-modify-seconds path)
)

(define (m_autoresize)
	(set-projection-transform (vector 1 0 0 0 
									  0 (/ (vx (get-screen-size)) (vy (get-screen-size))) 0 0 
									  0 0 -1 -1 
									  0 0 -2 0)
	)
)

(define (m_initaudio)
	(start-audio "system:capture_1" 512 44100)
)

(define (m_camlookat eye target up)
    (let* ([zaxis (vnormalise(vsub eye target))]
            [xaxis  (vnormalise(vcross up zaxis))] 
            [yaxis (vcross zaxis xaxis)]
            [orientation (vector (vx xaxis) (vx yaxis) (vx zaxis) 0
                    (vy xaxis) (vy yaxis) (vy zaxis) 0
                    (vz xaxis) (vz yaxis) (vz zaxis) 0
                    0 0 0 1)]
            [translation (mtranslate (vector (- (vx eye)) (- (vy eye)) (- (vz eye)) 1))]
            [camera-matrix (mmul orientation translation)])
        (set-camera camera-matrix)))

(define (m_poltocar r a)
    (vector (* r (cos a)) (* r (sin a)) 0))

(define (m_initosc host port)
	(osc-source port)
	;(osc-destination (string-append "osc.udp://" host ":" port))
)


;https://github.com/gaborpapp/LDS-fluxus/blob/master/final/01-02-give_my_try_a_love_%2B_i_want_to_break_free.scm
;returns the world position of the grabbed primitive
(define (m_getpos)
    (let ([t (get-global-transform)])
        (vector (vector-ref t 12)
            (vector-ref t 13)
            (vector-ref t 14))))

(define (m_pripos pri)
	(with-primitive pri
		(m_getpos)
	)
)

(define (m_gh n) ;exclude amp gh
    (+ 1 (gh n))
)

(define (m_gl n) ;log gh
    (log (+ 1 (gh n)))
)

(define (m_initmidi)
	(midiin-open 0)
)

(define (m_midicc id)
	(midi-ccn 0 id)
)

(define (m_windowsize w h)
	 (set-screen-size (vector w h)) ; 
	 (m_autoresize)
)