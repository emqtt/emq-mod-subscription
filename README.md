emq_mod_subscription
====================

Subscription Management Module

Configure Subscription Module
-----------------------------

etc/plugins/emq_mod_subscription.conf

```
## Subscribe the Topics automatically when a client connected
module.subscription.1.topic = $client/%c
## Qos of the subscription: 0 | 1 | 2
module.subscription.1.qos = 1

##module.subscription.2.topic = $user/%u
##module.subscription.2.qos = 1
```

Load Subscription Module
------------------------

Note: This module will be loaded by default.

```
./bin/emqttd_ctl plugins load emq_mod_subscription
```

License
-------

Apache License Version 2.0

Author
------

Feng Lee <feng at emqtt.io>

