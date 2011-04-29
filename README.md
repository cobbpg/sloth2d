Sloth2D physics
===============

This package provides a purely functional 2D physics library with
composable high-level abstractions.

Features
--------

* 100% pure implementation
* deterministic simulation (replayable regardless of sampling rate)
* convex colliders

Sore spots
----------

* extremely naive collision resolution

Planned features
----------------

* other collider shapes: concave, round, half-plane
* collision layers
* spatial hashing for more efficient collision detection
* object deactivation
* support for raycasting
* serialisation of physics state
* combinators on dynamic worlds
* constraints
* friction
* stacking
* a scene graph-based interface to define the world in a compact manner
