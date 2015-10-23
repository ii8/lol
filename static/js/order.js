
function _order_get()
{
    var name = "order=";
    var ca = document.cookie.split(';');
    for(var i=0; i < ca.length; i++) {
        var c = ca[i];

        while(c.charAt(0) == ' ')
            c = c.substring(1, c.length);

        if(c.indexOf(name) == 0) {
            var str = c.substring(name.length, c.length);
            try {
                return JSON.parse(str);
            } catch(e) {
                break;
            }
        }
    }
    document.cookie = "order=[]";
    return [];
}

function _order_put(order)
{
    str = JSON.stringify(order);
    document.cookie = "order=" + str;
}

function _order_mod(rel, p, q)
{
    var cookie = _order_get();

    for(var i=0; i < cookie.length; i++) {
        var c = cookie[i];

        if(p == c.product) {
            if(rel)
                c.quantity += q;
            else
                c.quantity = q;

            if(c.quantity <= 0)
                cookie.splice(i, 1);
            _order_put(cookie);
            return;
        }
    }
    if(q <= 0)
        return;
    cookie.push({product: p, quantity: q});
    _order_put(cookie);
}

function order_mod(p, q)
{
    _order_mod(true, p, q);
}

function order_set(p, q)
{
    _order_mod(false, p, q);
}

function order_inc(product)
{
    order_mod(product, 1);
}

function order_dec(product)
{
    order_mod(product, -1);
}
