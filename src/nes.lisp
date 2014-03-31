(defpackage :romreader-nes
  (:use :cl :romreader))

(in-package :romreader-nes)

;;;;; NES file format docs: http://fms.komkon.org/EMUL8/NES.html#LABM
;;;;; A little more NES ROM talk: http://sadistech.com/nesromtool/romdoc.html

;;;; Mappers

(defvar *mapper-table*
  '((0  . "No mapper")            ; All 32kB ROM + 8kB VROM games
    (1  . "Nintendo MMC1")        ; Megaman2, Bomberman2, etc.
    (2  . "CNROM switch")         ; Castlevania, LifeForce, etc.
    (3  . "UNROM switch")         ; QBert, PipeDream, Cybernoid, many Japanese games
    (4  . "Nintendo MMC3")        ; SilverSurfer, SuperContra, Immortal, etc.
    (5  . "Nintendo MMC5")        ; Castlevania3
    (6  . "FFE F4xxx")            ; F4xxx games off FFE CDROM
    (7  . "AOROM switch")         ; WizardsAndWarriors, Solstice, etc.
    (8  . "FFE F3xxx")            ; F3xxx games off FFE CDROM
    (9  . "Nintendo MMC2")        ; Punchout
    (10 . "Nintendo MMC4"))       ; Punchout 2
  "A list of known NES Memory Mappers of the form (Number . Name).")

;;;; Parser

(defun parse-header (byte-vector)
  (if (and (equalp #(78 69 83 26) (subseq byte-vector 0 4)) ; "NES^Z"
           (every #'zerop (subseq byte-vector 10 16)))
      ;; The mapper's four low bits are at the end of byte 6
      ;; and the corresponding high bits are at the end of byte 7.
      (let ((mapper-id (+ (ash (ldb (byte 4 4) (aref byte-vector 7)) 4)
                          (ldb (byte 4 4) (aref byte-vector 6)))))
        (list :prg-roms (aref byte-vector 4) ;; program rom
              :prg-size (* #x4000 (aref byte-vector 4))
              :chr-roms (aref byte-vector 5) ;; character rom
              :chr-size (* #x2000 (aref byte-vector 5))
              :8k-rams (let ((byte (aref byte-vector 8)))
                         (if (zerop byte) 1 byte)) ; backwards compatibility
              :mapper-id mapper-id
              :mapper-name (or (rest (assoc mapper-id *mapper-table*)) "Unknown")
              :region (case (aref byte-vector 9)
                        (0 :ntsc)
                        (1 :pal)
                        (t (error 'malformed-header
                                  :message "Region must be 0 or 1.")))
              :mirroring (if (zerop (ldb (byte 1 0) (aref byte-vector 6)))
                             :horizontal
                             :vertical)
              :battery-ram-p (zerop (ldb (byte 1 1) (aref byte-vector 6)))
              :trainer-p (zerop (ldb (byte 1 2) (aref byte-vector 6)))
              :four-screen-vram-p (zerop (ldb (byte 1 3) (aref byte-vector 6)))
              :vs-cartridge-p (let ((byte (aref byte-vector 7)))
                                (if (zerop (ldb (byte 3 1) byte))
                                    (zerop (ldb (byte 1 0) byte))
                                    (error 'malformed-header
                                           :message "Non-0 bits in byte 7.")))))
      (error 'malformed-header :message "NES^Z or zero pad bytes missing.")))

(defreader "nes"
  (list (parse-header (coerce (loop for i from 0 to 15
                                 collecting (read-byte in)) 'vector))
        (coerce (loop for byte = (read-byte in nil)
                   while byte collect byte) 'vector)))

(defmethod initialize-instance :after ((rom nes) &key)
  (with-accessors ((meta rom-metadata)
                   (bin rom-binary)) rom
    (let ((prg-length (* #x4000 (getf meta :prg-roms)))
          (chr-length (* #x2000 (getf meta :chr-roms))))
      (setf (rom-prg rom) (subseq bin 0 prg-length)
            ; Note: If chr is never followed, (subseq bin prg-length) is enough.
            (rom-chr rom) (subseq bin prg-length (+ prg-length chr-length))))))
