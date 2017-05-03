/**
 * file name : Validateable.java
 * created at : ����5:03:10 2016��8��25��
 * created by 970655147
 */

package com.hx.log.validator.interf;

import com.hx.log.interf.Result;

// Validateable, �о� ������ǰ���Ǹ�Validator����['HXBlog' in 'BaseAction']
public interface Validator<T> {


    /**
     * У������Ĳ���, �ж��Ƿ�Ϸ�
     *
     * @param obj   ��ҪУ��Ķ���
     * @param extra ���Ӳ���
     * @return true if args is valid, or esle
     * @author Jerry.X.He
     * @date 4/12/2017 10:19 PM
     * @since 1.0
     */
    Result validate(T obj, Object extra);

}
