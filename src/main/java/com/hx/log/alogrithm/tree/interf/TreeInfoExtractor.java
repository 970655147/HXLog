package com.hx.log.alogrithm.tree.interf;

import com.hx.json.JSONObject;

/**
 * TreeInfoExtractor
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 9:26 PM
 */
public interface TreeInfoExtractor<T> {

    /**
     * ��bean�е�����, ��ȡ��obj��
     *
     * @param bean ������bean
     * @param obj  �������ݵ�JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 9:26 PM
     * @since 1.0
     */
    void extract(T bean, JSONObject obj);

}
