Toy ray tracer based on the [Ray Tracing in One Weekend
series](https://raytracing.github.io).

# Ideas

- [ ] Fix overuse of `&key` arguments, particularly with custom structure
      constructors.
- [ ] I'm not sure, but I think Perlin textures have been broken by something
      somewhere.
- [ ] Camera simplification: remove all the time stuff, that can be handled by
      scene construction. Remove all the supersampling stuff, or at least
      refactor it: rather than having tricky stratified sampling within a
      pixel, just render a larger image (or decouple rendering resolution from
      pixel resolution, so we don't need to store the massive bitmap but can
      collapse instead the `n` render pixels down to the single image pixel
      earlier.
- [ ] Optimise e.g. vec.lisp has loads of actionable notes, and the rest could
      stand profiling.
- [ ] Nicer randomness: output seed at start of render, and ensure output is
      deterministic for a given seed.
- [ ] Threads. The book discusses an approach of having each thread render the
      entire image with a different random seed, whereas I would have naïvely
      tried to have some kind of parallel map of ray-colour or something.
- [ ] Fix coördinate transformations: at the moment we have two ways of having
      a sphere centred at x,y: either just put it there, or put it somewhere
      else then wrap it in a `translate`. I think just having the latter would
      be nice. We also have the fun of `rotate-y` only working in the one
      axis, surely we can just use transformation matrices here and make
      everything simpler?
- [ ] Scene compilation: the simplest idea here is flattening nested vectors,
      but maybe other things could be done? Do this after playing with
      coördinate transforms as mentioned above. Also, having to manually
      identify the lights is kinda annoying.
- [ ] HDR images (output & textures)
- [ ] "Proper" gamma support (input & output)
