package com.hx.log.test;

import com.hx.log.json.JSONTransferableUtilsConstants;
import com.hx.log.util.Constants;
import org.junit.Test;

import static com.hx.log.util.Log.info;

/**
 * Test16ConsstantsOpt
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/1/2017 12:47 AM
 */
public class Test16ConsstantsOpt {

    @Test
    public void optOptions() {

        info(Constants.optString(Constants._REFLECT_SETTER_PREFIX));
        info(Constants.optString(JSONTransferableUtilsConstants._JSON_TBEAN_KEY));

        info(Constants.optString("hxJson.cache.capacity"));

    }

}
