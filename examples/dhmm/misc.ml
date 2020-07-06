open Probzelus
open Types

let () = Random.init 42

let uniform_3  =
  let draw () =
    let p1 = Distribution.draw (Distribution.uniform_float (0.1, 1.1)) in
    let p2 = Distribution.draw (Distribution.uniform_float (0.1, 1.1)) in
    let p3 = Distribution.draw (Distribution.uniform_float (0.1, 1.1)) in
    let s = p1 +. p2 +. p3 in
    (p1 /. s, p2 /. s, p3 /. s)
  in
  let score x = assert false in
  Distribution.sampler (draw, score)


let triple (m1, m2, m3) =
  let draw () =
    let p1 = Distribution.draw (Distribution.gaussian (m1, 0.001)) in
    let p2 = Distribution.draw (Distribution.gaussian (m2, 0.001)) in
    let p3 = Distribution.draw (Distribution.gaussian (m3, 0.001)) in
    let s = p1 +. p2 +. p3 in
    (p1 /. s, p2 /. s, p3 /. s)
  in
  let score x = assert false in
  Distribution.sampler (draw, score)
