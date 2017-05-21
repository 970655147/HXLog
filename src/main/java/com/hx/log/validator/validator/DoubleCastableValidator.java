package com.hx.log.validator.validator;


import com.hx.common.interf.common.Result;
import com.hx.common.util.ResultUtils;
import com.hx.common.interf.validator.Validator;

/**
 * DoubleCastableValidator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 9:35 PM
 */
public class DoubleCastableValidator implements Validator<Object> {

    /**
     * 单例
     */
    private static DoubleCastableValidator INSTANCE;

    /**
     * 初始化
     *
     * @since 1.0
     */
    private DoubleCastableValidator() {

    }

    /**
     * 获取单例对象
     *
     * @return com.hx.log.validator.validator.BooleanCastableValidator
     * @author Jerry.X.He
     * @date 5/6/2017 5:26 PM
     * @since 1.0
     */
    public static DoubleCastableValidator getInstance() {
        if(INSTANCE == null) {
            synchronized (DoubleCastableValidator.class) {
                if(INSTANCE == null) {
                    INSTANCE = new DoubleCastableValidator();
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

        if ((obj instanceof Byte) || (obj instanceof Short)
            || (obj instanceof Integer) || (obj instanceof Long)
            || (obj instanceof Float) || (obj instanceof Double) ) {
            return ResultUtils.success();
        }
        if (obj instanceof String) {
            String str = (String) obj;
            try {
                Double.valueOf(str);
                return ResultUtils.success();
            } catch (Exception e) {
                // ignore
            }
        }

        return failed(obj);
    }

    /**
     * 比较失败之后返回的结果
     *
     * @return com.hx.common.result.SimpleResult
     * @author Jerry.X.He
     * @date 5/3/2017 9:40 PM
     * @since 1.0
     */
    private Result failed(Object obj) {
        return ResultUtils.failed("the obj : " + obj + " can't cast to double !");
    }

}
