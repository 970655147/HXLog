/**
 * file name : JSONTransferable.java
 * created at : 4:52:00 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.util;

import java.util.Map;
import java.util.Set;

import net.sf.json.JSONObject;

// bean ��json�ַ���֮���໥ת���Ľӿ�
public interface JSONTransferable<BeanType extends JSONTransferable<BeanType, IdxType>, IdxType> {
	
	// ��json�ַ�����bean֮���ת��
	// ��bean��json�ַ���֮���ת��
	// �����ҵ���߼��ɿ������й�Լ, �������idxMap��Ҫ���������ڻ�ȡ��ǰ������һ������, ������
	public BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap);
	public JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap);
	// for cycleDectector, 	 add at 2016.06.19
	public JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap, Set<Object> cycleDectector);
	
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
}
