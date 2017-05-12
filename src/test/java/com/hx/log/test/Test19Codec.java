package com.hx.log.test;

import com.hx.log.alogrithm.code.Codec;
import com.hx.log.util.Tools;
import org.junit.Test;

import static com.hx.log.util.Log.info;

/**
 * $TODO
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/12/2017 10:35 PM
 */
public class Test19Codec {

    @Test
    public void base64Encode() {

        String msg = "Hello World !";

        byte[] encoded = Codec.base64E(msg.getBytes(), Tools.UTF_8);
        info(Codec.newString(encoded, Tools.UTF_8));

        byte[] decoded = Codec.base64D(encoded, Tools.UTF_8);
        info(Codec.newString(decoded, Tools.UTF_8));

    }


}
