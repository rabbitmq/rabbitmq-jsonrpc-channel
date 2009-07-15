//   The contents of this file are subject to the Mozilla Public License
//   Version 1.1 (the "License"); you may not use this file except in
//   compliance with the License. You may obtain a copy of the License at
//   http://www.mozilla.org/MPL/
//
//   Software distributed under the License is distributed on an "AS IS"
//   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
//   License for the specific language governing rights and limitations
//   under the License.
//
//   The Original Code is RabbitMQ.
//
//   The Initial Developers of the Original Code are LShift Ltd,
//   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
//
//   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
//   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
//   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
//   Technologies LLC, and Rabbit Technologies Ltd.
//
//   Portions created by LShift Ltd are Copyright (C) 2007-2009 LShift
//   Ltd. Portions created by Cohesive Financial Technologies LLC are
//   Copyright (C) 2007-2009 Cohesive Financial Technologies
//   LLC. Portions created by Rabbit Technologies Ltd are Copyright
//   (C) 2007-2009 Rabbit Technologies Ltd.
//
//   All Rights Reserved.
//
//   Contributor(s): ______________________________________.
//

JsonRpcRequestId = 1;

//
// Add Prototype-like context binding. Taken from http://www.quirkey.com/blog/2009/02/25/switched-to-jquery
//
jQuery.extend({
    shove: function(fn, object) {
        return  function() { return fn.apply(object, arguments); };
    },

    toArray: function(iterable) {
        var result = new Array(iterable.length);
        var length = iterable.length;
        while (length--) result[length] = iterable[length];
        return result;
    }
});

var JsonRpcTransaction = function(serviceUrl, methodName, params, options) {
    this.buildRequest = function() {
        return { version: "1.1",
             id: JsonRpcRequestId++,
             method: this.methodName,
             params: this.params };
    };

    this.sendRequest = function() {
        var headers = [];
        if (this.options.timeout) {
            headers.push('X-JSON-RPC-Timeout', this.options.timeout);
        }
        var req = this.buildRequest();
        //this.debugLog({requestX: req});
        this.request =
            new jQuery.ajax(
                     { url:             this.serviceUrl,
                       type:            'post',
                       beforeSend:      function(request) {
                                            for (var i = 0; i < headers.length; i += 2)
                                                request.setRequestHeader(headers[i], headers[i+1])
                                        },
                       contentType:     'application/json',
                       accepts:         {json: 'application/json'},
                       data:            JSON.stringify(req),
                       complete:        jQuery.shove(this.receiveReply, this) 
                     });
    }

    this.debugLog = function(x) {
        if (this.options.debug) {
            this.options.debugLogger(x);
        }
    };

    this.receiveReply = function(ajaxRequest) {
        var response = JSON.parse(ajaxRequest.responseText);
        //this.debugLog({responseX: response});
        if (response.error) {
            if (this.options.debug) {
            this.debugLog("JsonRPC error:" +
                      "\nService: " + JSON.stringify(this.serviceUrl) +
                      "\nMethod: " + JSON.stringify(this.methodName) +
                      "\nParams: " + JSON.stringify(this.params) +
                      "\nResponse: " + JSON.stringify(response).replace(/\\n/g, "\n"));
            }

            this.error = response.error;
            jQuery.each(this.errorCallbacks, function () {
                         try { this(response.error, true); }
                         catch (err) {}
                         });
        } else {
            var reply = response.result;
            this.reply = reply;
            this.replyReady = 1;
            jQuery.each(this.callbacks, function () {
                        try { this(reply, false); }
                        catch (err) {
                            if (console) console.error(err);
                        }
                    });
        }
    };

    this.addReplyTransformer = function(xformer) {
        var oldAddCallback = jQuery.shove(this.addCallback, this);
        this.addCallback = function(cb) {
            return oldAddCallback(function(reply, is_error) {
                          cb(is_error ? reply : xformer(reply), is_error);
                      });
        }
        return this;
    };

    this.addCallback = function(cb) {
        this.callbacks.push(cb);
        if (this.replyReady) {
            try { cb(this.reply, false); }
            catch (err) {}
        }
        return this;
    };

    this.addErrorCallback = function(cb) {
        this.errorCallbacks.push(cb);
        if (this.error) {
            try { cb(this.error, true); }
            catch (err) {}
        }
        return this;
    }

    this.options = {
        debug: false,
        debugLogger: alert,
        timeout: 0 /* milliseconds; zero means "do not specify" */
    };
    jQuery.extend(this.options, options || {});
    this.serviceUrl = serviceUrl;
    this.methodName = methodName;
    this.params = params;
    this.error = null;
    this.reply = null;
    this.replyReady = 0;
    this.callbacks = [];
    this.errorCallbacks = [];
    this.sendRequest();
}

JsonRpcService = function(serviceUrl, onReady, options) {
    this.installGenericProxy = function(desc) {
        if (this.options.debug) {
            this.options.debugLogger({installGenericProxy: desc});
        }
        this[desc.name] = function () {
            var actuals = jQuery.toArray(arguments);
            while (actuals.length < desc.params.length) {
            actuals.push(null);
            }
            return new (this.options.transactionClass)(this.serviceUrl,
                                   desc.name,
                                   actuals,
                                   {
                                   debug: this.options.debug,
                                   debugLogger: this.options.debugLogger,
                                   timeout: this.options.timeout
                                   });
        };
    };

    this.options = {
        transactionClass: JsonRpcTransaction,
        timeout: 0, /* milliseconds; zero means "do not specify" */
        debug: false,
        debugLogger: alert
    };
    jQuery.extend(this.options, options || {});
    this.serviceUrl = serviceUrl;
    var svc = this;
    var txn = new (this.options.transactionClass)(serviceUrl,
                              "system.describe",
                              [],
                              {debug: this.options.debug,
                               debugLogger: this.options.debugLogger});
    txn.addCallback(receiveServiceDescription);
    function receiveServiceDescription(sd) {
        svc.serviceDescription = sd;
        jQuery.each(svc.serviceDescription.procs, function(i) { svc.installGenericProxy.apply(svc, [this]) });
        onReady();
    }
}
