/**
 * file name : JSONTransferable.java
 * created at : 4:52:00 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.interf;

import java.util.Deque;
import java.util.Map;
import java.util.Set;

import net.sf.json.JSONObject;

// bean ��json�ַ���֮���໥ת���Ľӿ�
public interface JSONTransferable<BeanType extends JSONTransferable<BeanType, IdxType>, IdxType> {

    // ��json�ַ�����bean֮���ת��
    // ��bean��json�ַ���֮���ת��
    // �����ҵ���߼��ɿ������й�Լ, �������idxMap��Ҫ���������ڻ�ȡ��ǰ������һ������, ������

    /**
     * �Ӹ�����JSONObject�Լ���������ӳ�䴴��һ��Bean����
     *
     * @param obj    ������Map
     * @param idxMap ��������������ӳ��
     * @return bean that constructed by obj
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap);

    // for do not initialize 'Object' except 'int, .., String, JSONObject, JSONArray'			add at 2016.06.20

    /**
     * �Ӹ�����JSONObject�Լ���������ӳ�䴴��һ��Bean����
     *
     * @param obj           ������Map
     * @param idxMap        ��������������ӳ��
     * @param initObjFilter ����Ҫ�ݹ�loadFromJSON������
     * @return bean that constructed by obj
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap, Set<String> initObjFilter);

    /**
     * ����ǰBean���ݸ�������������ӳ�� ����Ҫ���˵�����ӳ�乹��һ��JSONObject��ʾ
     *
     * @param idxMap       ������Map
     * @param filterIdxMap ��������������ӳ��
     * @return JSONObject represent current JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap);

    // for cycleDectector, 		 add at 2016.06.19

    /**
     * ����ǰBean���ݸ�������������ӳ�� ����Ҫ���˵�����ӳ�乹��һ��JSONObject��ʾ
     *
     * @param idxMap         ������Map
     * @param filterIdxMap   ��������������ӳ��
     * @param cycleDectector ѭ������Set
     * @return JSONObject represent current JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap, Deque<Object> cycleDectector);

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

    // ��ȡbeanKey, �Լ�protoBean			add at 2016.06.18
    // Ĭ�ϵļ��ط�ʽ, Ĭ�ϵ�filter

    /**
     * ��ȡ��ǰ��JSONTransferable��beanKey, ��idxMap, filterIdxMapע��
     *
     * @return beanKey
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    String beanKey();

    /**
     * ��ȡ��ǰ��JSONTransferable��һ��Dummy����
     *
     * @return dummy of JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    BeanType protoBean();

    /**
     * ��ȡ��ǰ��JSONTransferable��Ĭ��idxMap��idx
     *
     * @return dummy of JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    IdxType defaultLoadIdx();

    /**
     * ��ȡ��ǰ��JSONTransferable��Ĭ��filterIdxMap��idx
     *
     * @return dummy of JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    IdxType defaultFilterIdx();

    /**
     * ���õ�ǰJSONTransferable�ĸ��������Ե�ֵ
     *
     * @param attr ���������Ե�����
     * @param val  ���������Ե�ֵ
     * @return set `attr`'s value to be `val`
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    JSONTransferable<BeanType, IdxType> set(String attr, Object val);

    // for add element to 'Collection' 				add at 2016.06.20
    /**
     * Ϊ�����������һ������
     *
     * @param attr ���������Ե�����
     * @param val  ���������Ե�ֵ
     * @return set `attr`'s value to be `val`
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    JSONTransferable<BeanType, IdxType> add(String attr, Object val);
}
