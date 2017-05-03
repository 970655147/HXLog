package com.hx.log.test;

import com.hx.json.JSONObject;
import com.hx.log.interf.Result;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.log.validator.ValidatorUtils;
import org.junit.Test;

import static com.hx.log.util.Log.info;

/**
 * Test17Validator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 10:28 PM
 */
public class Test17Validator {

    @Test
    public void test01ValidatorRegister() {

//        Result result = ValidatorUtils.register().register("1", ValidatorUtils.eq(2)).apply();
        Result result = ValidatorUtils.register()
                .register(1, ValidatorUtils.eq(1))
                .register(12, ValidatorUtils.gt(10))
                .apply();

        info(JSONObject.fromObject(result).toString() );

    }

}
