package com.hx.log.test;

import com.hx.common.str.WordsSeprator;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;
import org.junit.Test;

import static com.hx.log.util.Log.info;
import static com.hx.log.util.Log.infoHorizon;

/**
 * Test15WordsSeprator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 3:20 PM
 */
public class Test15WordsSeprator {

    @Test
    public void test01ForPos() {

        Log.infoFatalLogger.logPatternChain = null;

        String str = "123|456|789|0|";
        WordsSeprator sep = new WordsSeprator(str, Tools.asSet("|"), null, true);

        while(sep.hasNext()) {
            info(sep.seek());
            info(sep.next());
            info(sep.current());
            info(sep.prev());

            info(sep.currentStartIdx());
            info(sep.prevStartIdx());
            info(sep.rest());
            infoHorizon();
        }

    }

}
