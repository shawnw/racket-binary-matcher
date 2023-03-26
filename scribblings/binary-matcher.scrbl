#lang scribble/manual
@require[@for-label[binary-matcher
                    racket/base racket/contract net/ip]]

@title{Destructuring bytestrings using match}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@defmodule[binary-matcher]

This module introduces a new @code{match} pattern for matching and destructuring binary data encoded in a bytestring.

The API should be considered very alpha and open to incompatible changes.

@section{The binary match pattern}

@defform[
         #:literals (binary bytes zero-padded until-byte until-byte* length-prefixed
                            get-offset set-offset!
                            s8 u8 s16 u16 s32 u32 s64 u64 f32 f64
                            rest* big-endian little-endian native-endian)
         (binary byte-pattern ...+ maybe-rest)
         #:grammar
         [(byte-pattern (bytes pat length)
                        (code:line (zero-padded pat length))
                        (code:line (until-byte pat byte))
                        (code:line (until-byte* pat byte))
                        (code:line (length-prefixed pat))
                        (code:line (length-prefixed pat prefix-length endianness))
                        (code:line (number-type pat))
                        (code:line (number-type pat endianness))
                        (code:line control-pattern))
          (maybe-rest (code:line)
                      (code:line (rest* pat)))
          (control-pattern (get-offset pat)
                           (code:line (set-offset! offset)))
          (number-type s8
                       (code:line u8)
                       (code:line s16)
                       (code:line u16)
                       (code:line s32)
                       (code:line u32)
                       (code:line u64)
                       (code:line s64)
                       (code:line f32)
                       (code:line f64))
          (prefix-length u8
                         (code:line u16)
                         (code:line u32)
                         (code:line u64))
          (endianness big-endian
                      (code:line little-endian)
                      (code:line native-endian))]

 #:contracts
 [(byte byte?)
  (length (and/c fixnum? positive?))
  (offset (and/c fixnum? (>=/c 0)))]]{

 A @code{match} extender that, when matched against a bytestring, tries to destructure it according to the given spec and match extracted values against given match patterns.

 An example:

 @codeblock{
            (match #"\17\240bc"
              ((binary (s16 num big-endian) (bytes rest 2))
               (list num rest))) ; (4000 #"bc")
            }

@code{bytes} extracts a fixed-width field. @code{zero-padded} extracts a fixed-width field and strips trailing 0 bytes. @code{until-byte} extracts bytes until the given
delimiter byte is encountered. @code{until-byte*} is the same but a failure to find the delimiter is not a match failure. @code{length-prefixed} reads a length header and then
that many bytes. It defaults to the 9P specification of a 2 byte big-endian length if not explicitly specified.

The number patterns should hopefully be self explanatory.

@code{rest*} takes any remaining bytes at the end of the bytestring after everything else is matched; if there are no extra bytes, it applies an empty bytestring
to its pattern.

Normally, matching starts with the first byte in the bytestring. @code{(set-offset! where)} changes the location (To facilitate matching bytestrings with multiple records),
and @code{get-offset} will save the current index at that point in the matching.

A more complex example, that matches an IPv4 header:

@codeblock{
   (parameterize ([binary-match-default-endianness 'network-order])
     (match header
       ((binary
         (u8 (app byte->nybbles version header-length)) (u8 service-type) (u16 total-length)
         (u16 identification) (u16 flags+fragment)
         (u8 ttl) (u8 protocol) (u16 checksum)
         (bytes (app make-ip-address source-address) 4)
         (u32 (app (lambda (n) (make-ip-address n 4)) dest-address))
         (rest* options))
        (list version header-length service-type total-length ttl protocol
              (ip-address->string source-address) (ip-address->string dest-address)
              options))))
        }
}

@section{Additional functions}

@defproc[(byte->nybbles [b byte?]) (values byte? byte?)]{

 Splits a single byte into two 4-bit nybbles. The upper 4 bits is the first value, the lower 4 is the second.

 }

@defparam*[binary-match-default-endianness endianness
           (or/c 'big-endian 'little-endian 'native-endian 'network-order 'host-order)
           (or/c 'big-endian 'little-endian 'native-endian)
           #:value 'native-endian]{

 A parameter that controls the endianness used by numeric patterns when one isn't explicitly given.
                                                             
}
