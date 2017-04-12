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
	public BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap);
	// for do not initialize 'Object' except 'int, .., String, JSONObject, JSONArray'			add at 2016.06.20
	public BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap, Set<String> initObjFilter);
	public JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap);
	// for cycleDectector, 		 add at 2016.06.19
	public JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap, Deque<Object> cycleDectector);
	
	// ����һ����ǰ������ʵ��[����BaseDao]
	public BeanType newInstance(Object... args);
	public String id();
	
	// ��ȡbeanKey, �Լ�protoBean			add at 2016.06.18
	// Ĭ�ϵļ��ط�ʽ, Ĭ�ϵ�filter
	public String beanKey();
	public BeanType protoBean();
	public IdxType defaultLoadIdx();
	public IdxType defaultFilterIdx();
	public JSONTransferable<BeanType, IdxType> set(String attr, Object val);
	// for add element to 'Collection' 				add at 2016.06.20
	public JSONTransferable<BeanType, IdxType> add(String attr, Object val);
}
