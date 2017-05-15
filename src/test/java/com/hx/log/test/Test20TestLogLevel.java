package com.hx.log.test;

import com.hx.log.log.LogLevel;
import com.hx.log.util.Log;
import org.junit.Test;

/**
 * Test20TestLogLevel
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/15/2017 9:04 PM
 */
public class Test20TestLogLevel {

    @Test
    public void testLogLevel() {

        Log.LOG_LEVEL_MIN = LogLevel.ERR;

        Log.info("sdfsdf");

        Log.err("sdfgdf11111111g");
        Log.fatal("sdfgdf11111111g");

        Log.fatal("sdfgdf1111 {} 1111g", "doIt", "dd");

    }

}
