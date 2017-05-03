/**
 * file name : Validateable.java
 * created at : 下午5:03:10 2016年8月25日
 * created by 970655147
 */

package com.hx.log.validator.interf;

import com.hx.log.interf.Result;

// Validateable, 感觉 和我以前的那个Validator好像啊['HXBlog' in 'BaseAction']
public interface Validator<T> {


    /**
     * 校验给定的参数, 判断是否合法
     *
     * @param obj   需要校验的对象
     * @param extra 附加参数
     * @return true if args is valid, or esle
     * @author Jerry.X.He
     * @date 4/12/2017 10:19 PM
     * @since 1.0
     */
    Result validate(T obj, Object extra);

}
