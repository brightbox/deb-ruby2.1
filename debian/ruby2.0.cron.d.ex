#
# Regular cron jobs for the ruby2.0 package
#
0 4	* * *	root	[ -x /usr/bin/ruby2.0_maintenance ] && /usr/bin/ruby2.0_maintenance
