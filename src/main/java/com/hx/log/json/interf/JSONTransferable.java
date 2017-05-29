/**
 * file name : JSONTransferable.java
 * created at : 4:52:00 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.json.interf;

import com.hx.json.JSONObject;
import com.hx.json.config.interf.JSONConfig;

import java.util.Deque;
import java.util.Map;
import java.util.Set;

/**
 * bean ��json�ַ���֮���໥ת���Ľӿ�
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 8:09 AM
 */
public interface JSONTransferable<BeanType extends JSONTransferable<BeanType>> {

    // ��json�ַ�����bean֮���ת��
    // ��bean��json�ַ���֮���ת��
    // �����ҵ���߼��ɿ������й�Լ, �������idxMap��Ҫ���������ڻ�ȡ��ǰ������һ������, ������

    /**
     * �Ӹ�����JSONObject�Լ���������ӳ�䴴��һ��Bean����
     *
     * @param obj           ������Map
     * @param config        ����json��config
     * @return bean that constructed by obj
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    BeanType loadFromJSON(Map<String, Object> obj, JSONConfig config);

    /**
     * ����ǰBean���ݸ�������������ӳ�� ����Ҫ���˵�����ӳ�乹��һ��JSONObject��ʾ
     *
     * @param config ����json��config
     * @return JSONObject represent currentStartIdx JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(JSONConfig config);

    /**
     * ����ǰBean���ݸ�������������ӳ�� ����Ҫ���˵�����ӳ�乹��һ��JSONObject��ʾ
     * for cycleDectector, 		 add at 2016.06.19
     *
     * @param config   ����json��config
     * @param cycleDectector ѭ������Set
     * @return JSONObject represent currentStartIdx JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(JSONConfig config, Deque<Object> cycleDectector);

    // ����һ����ǰ������ʵ��[����BaseDao]
    BeanType newInstance(Object... args);

    /**
     * ��ȡ��ǰJSONTransferable��id
     *
     * @return id
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    String id();

    /**
     * ���õ�ǰJSONTransferable��id
     *
     * @return id
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    void id(String id);

}
