self: super:
if super ? prefer-remote-fetch then
  (super.prefer-remote-fetch self super)
else super
