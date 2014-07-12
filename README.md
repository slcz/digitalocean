digitalocean
============

digital ocean management tools API V2.

Usage:
	digitalocean [OPTIONS...] COMMAND

COMMAND:
	listimages   -- list all OS images
	listregions  -- list all regions
	listsizes    -- list all supported droplet sizes
	listdroplets -- list all droplets
	createdroplet-- create a new droplet
	deletedroplet-- delete a droplet

OPTIONS:
	-h --help -- help message
	-n --name -- droplet name
	-r --region -- region code
	-s --size -- size string
	-i --image -- image for droplet

Also need to set environmental variable TOKEN to digital ocean API token.
