package com.hx.log.validator.validator;


import com.hx.common.interf.common.Result;
import com.hx.log.util.Tools;
import com.hx.log.validator.ValidateResult;
import com.hx.log.validator.ValidateResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * BooleanCastableValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:35 PM
 */
public class BooleanCastableValidator implements Validator<Object> {

    /**
     * 单例
     */
    private static BooleanCastableValidator INSTANCE;

    /**
     * 初始化
     *
     * @since 1.0
     */
    private BooleanCastableValidator() {

    }

    /**
     * 获取单例对象
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:26 PM
     * @since 1.0
     */
    public static BooleanCastableValidator getInstance() {
        if(INSTANCE == null) {
            synchronized (BooleanCastableValidator.class) {
                if(INSTANCE == null) {
                    INSTANCE = new BooleanCastableValidator();
                }
            }
        }

        return INSTANCE;
    }

    @Override
    public Result validate(Object obj, Object extra) {
        if(obj == null) {
            return failed(obj);
        }

        if (obj instanceof Boolean) {
            return ValidateResultUtils.success();
        }
        if (obj instanceof String) {
            String str = (String) obj;
            if (Tools.equalsIgnoreCase(Tools.TRUE, str) || Tools.equalsIgnoreCase(Tools.FALSE, str)) {
                return ValidateResultUtils.success();
            }
        }

        return failed(obj);
    }

    /**
     * 比较失败之后返回的结果
     *
     * @return com.hx.log.validator.ValidateResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private ValidateResult failed(Object obj) {
        return ValidateResultUtils.failed("the obj : " + obj + " can't cast to boolean !");
    }

}
